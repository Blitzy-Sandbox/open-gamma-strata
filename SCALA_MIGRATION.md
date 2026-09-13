# Scala migration note — `strata-collect` and `strata-basics`

This note is the register of the Java-to-Scala port of two OpenGamma Strata modules: `strata-collect`
(the subset `strata-basics` actually uses) and `strata-basics`. It records, per the project's Agent
Action Plan (AAP) §0.8.3, what every `strata-collect` member that `strata-basics` used became, which
collect symbols were deliberately left behind, every behaviour that departs from the Java original,
the holiday calendars the port covers, the JSON shapes it reads and writes, and the commands that
build, demonstrate and gate it.

The port replaces Java 8 + Maven + Joda-Beans + Joda-Convert + Guava with Scala 2.13.18 + sbt on
JDK 21, using cats, cats-effect and circe. The Java sources under `modules/**` are **not** modified,
deleted or referenced by the sbt build; they remain the authority this port is measured against and
the source later slices are ported from. The two Scala modules contain no `.java` file, and neither
Guava, any Joda library nor the Java `strata-collect` artifact is on their classpath.

Two principles run through everything below:

- **Names are the contract.** Every ported `Named` type's `name`, `Show` and JSON string equal the
  Java `getName()` / Joda-Convert form — `Act/365F`, `EUR/USD`, `P3M`, `3M`, `GBLO+USNY`. Where a
  name or a rendered form differs from Java, it is a divergence row in section (c).
- **Failure is a value.** What Java signalled by throwing on bad *data* is returned as
  `Either[Failure, A]` or `EitherNec[Failure, A]` over a sealed `Failure` ADT; what Java threw to
  guard a *caller contract* (an index in bounds, dates in order, a non-negative size) stays a
  fail-fast `IllegalArgumentException` raised through `ArgCheck`. Section (c) lists every place the
  exception type or the failure channel visibly changed.

### How to read this note

Sections (a) to (f) are the six the AAP requires, in its order. Section (c) is the one to read before
depending on the port: it lists the behaviours that are deliberately not identical to Java, and the
one place where the AAP's own wording had to be reconciled with its authoritative file layout
(row 46). What supports each of its rows — which committed fixture, which spec, which command — is
stated at the head of that section. Section (g) is not one of
the six: it is the ledger of the build, CI and dependency findings raised against this tree and what
each one changed.

### What is in the tree

- **`strata-collect`** — the 18 main sources AAP §0.3.1 enumerates, and no others: validation
  (`ArgCheck`, `Validate`), the `Failure` ADT with `FailureReason` and the result aliases,
  `NamedEnum`, `Named`, the typed-string support (`TypedStringCompanion`), `Decimal` and
  `FixedScaleDecimal`, `DoubleArray`/`DoubleMatrix`/`Matrix`/`DoubleArrayMath`, `Collections`,
  `io.Resources`, `json.Codecs`. The two mechanisms that make the construction policy and the
  absence of Java serialization hold on the JVM rather than only in the Scala source — the
  construction guards every closed type runs, and the `NoJavaSerialization` refusal every product
  mixes in — are housed at the bottom of **`ArgCheck.scala`**, which is where they belong: they are
  fail-fast contract checks like everything else in that file, differing only in who the caller is,
  a constructor rather than a method. Both are `private[strata]`, so neither widens the public API
  of the module, and `JvmClosure` is not a `strata-basics`-specific concept — a later slice porting
  another module gets the same closure from the same place. Rows 42 and 43 describe what they do.
- **`strata-basics`** — 77 main sources: the root contracts (`ReferenceData`, `ReferenceDataId`,
  `Resolvable`, `CalculationTarget`, `StandardId`, `StandardSchemes`), `currency` (16), `date` (20,
  including `DayCount`, the 25 calendar generators, `THBA` and `StandardHolidayCalendars`), `index`
  (18, the sealed `Index` hierarchy with its four constants objects and four data tables,
  `FloatingRate`, `FloatingRateType`, `FloatingRateName`, `FloatingRateNameData`, the retained open
  interface contract `IndexObservation`, and its four observation implementations —
  `IborIndexObservation`, `OvernightIndexObservation`, `PriceIndexObservation` and
  `FxIndexObservation` — each in a file of its own), `location` (2), `schedule` (6), `value` (7)
  and `demo/BasicsDemoApp.scala`. That is the AAP's §0.3.1 layout file for file: the seven root
  sources plus 16 + 20 + 18 + 2 + 6 + 7 + 1.
- **Around them** — the six parity baselines, the reference-data manifest, the method-level
  `java-test-mapping.csv`, the capture tooling under `tools/parity-capture/`, the gate runner
  `scripts/verify-gates.sh`, and the tests: 21 test sources in `strata-collect` and 86 in
  `strata-basics`, whose counts the test-scope gate reads from the JUnit XML rather than from here.

Every reference-data table, alias table and lenient pattern of the Java modules is transcribed.
Section (a)'s inventory was regenerated from the compiled Java modules, so every `strata-collect`
member those modules reference carries either a replacement row or an explicit "no target" row with
the reason. Section (f) gives the commands.

## (a) Symbol table: every `strata-collect` member `strata-basics` uses

The Java `strata-basics` main sources import 29 distinct `strata-collect` targets (22 types plus 7
statically imported `Guavate` members); its test sources add three more (`TestHelper`,
`CollectProjectAssertions`, `Unchecked`). The table below is grouped by exactly those 32 targets and
by nothing else, and carries one row per member used, or per group of overloads whose replacement is
identical. **85 rows.** "No target" means the member's job disappeared with the mechanism it served,
and the row says what does the job now.

The member list is not read off the import lines, which name only the type: it was regenerated from
the compiled Java modules, by reading every `strata-collect` entry out of the constant pools of
`strata-basics-2.12.74-SNAPSHOT.jar` (311 classes) and `strata-basics-2.12.74-SNAPSHOT-tests.jar`
(90 classes) with `javap -v -p`. That is why instance members appear here and not only statics, and
it is what makes the table checkable: every member group either has a replacement row or a "no
target" row. Members of those types that Java `strata-basics` does **not** reference are not in this
table — they are in the supplemental table that follows it, which is labelled as being outside the
required inventory, as is the table of the port's own published additions after that.

Every replacement lives in `strata-collect` unless the Module column says otherwise; "collect (test)"
is the module's test scope, visible to `strata-basics` tests through
`dependsOn(strata-collect % "test->test")`.

| # | Import target | Member used | Scala replacement | Module |
|---|---------------|-------------|-------------------|--------|
| 1 | `collect.ArgChecker` | `isTrue` (both overloads) | `ArgCheck.isTrue` (fail-fast) / `Validate.isTrue` (accumulating) | collect |
| 2 | `collect.ArgChecker` | `isFalse` | `ArgCheck.isFalse` / `Validate.isFalse` | collect |
| 3 | `collect.ArgChecker` | `matches` | `ArgCheck.matches` / `Validate.matches` | collect |
| 4 | `collect.ArgChecker` | `notEmpty` — the `String` overload, the only one `strata-basics` calls (`MarketTenor.parse`) | `ArgCheck.notEmpty` / `Validate.notEmpty`, `String` overload. The overload sets are **not** identical: `ArgCheck.notEmpty` has eight forms (`String`, `Array[T]`, `Array[Int]`, `Array[Long]`, `Array[Double]`, `Iterable`, `Map`, `Matrix` — `ArgCheck.scala:326`), `Validate.notEmpty` the same set without the `Matrix` form | collect |
| 5 | `collect.ArgChecker` | `notNaN` | `ArgCheck.notNaN` / `Validate.notNaN` | collect |
| 6 | `collect.ArgChecker` | `notNegative` | `ArgCheck.notNegative` / `Validate.notNegative` | collect |
| 7 | `collect.ArgChecker` | `notNegativeOrZero` | `ArgCheck.notNegativeOrZero` / `Validate.notNegativeOrZero` | collect |
| 8 | `collect.ArgChecker` | `inOrderNotEqual` | `ArgCheck.inOrderNotEqual` / `Validate.inOrderNotEqual` | collect |
| 9 | `collect.ArgChecker` | `inOrderOrEqual` | `ArgCheck.inOrderOrEqual` / `Validate.inOrderOrEqual` | collect |
| 10 | `collect.ArgChecker` | `notNull`, `noNulls`, `notNullItem` | No target — see divergence (c)-3 | — |
| 11 | `collect.Decimal` | `of` (`long`, `double`, `BigDecimal`, `String`) | `Decimal.of` over the same four inputs, and `Decimal.parse` for text, each returning `Either[Failure, Decimal]`; `Decimal.ofScaled` builds from an unscaled value and a scale | collect |
| 12 | `collect.Decimal` | `ZERO` | `Decimal.ZERO` (with `MAX_VALUE`, `MIN_VALUE`) | collect |
| 13 | `collect.Decimal` | `plus`, `minus`, `multipliedBy`, `negated` | The same four members, total, each over a `Decimal`, `Long` or `Double` operand | collect |
| 14 | `collect.Decimal` | `isGreaterThan`, `isGreaterThanEqualTo`, `isLessThan`, `isLessThanEqualTo` | The same four members, plus `Order[Decimal]` for sorted collections | collect |
| 15 | `collect.Decimal` | `isZero` | `Decimal.isZero` | collect |
| 16 | `collect.Decimal` | `unscaledValue` | `Decimal.unscaledValue` | collect |
| 17 | `collect.Decimal` | `doubleValue` | `Decimal.doubleValue` | collect |
| 18 | `collect.Decimal` | `toBigDecimal` | `Decimal.toBigDecimal` | collect |
| 19 | `collect.Decimal` | `mapAsBigDecimal` | `Decimal.mapAsBigDecimal`, returning `Either[Failure, Decimal]` because the mapped value can leave the representable range | collect |
| 20 | `collect.Decimal` | `roundToScale` | `Decimal.roundToScale(desiredScale, roundingMode)` (with `roundToPrecision`) | collect |
| 21 | `collect.Decimal` | `formatAtLeast` | `Decimal.formatAtLeast` (with `format(decimalPlaces, roundingMode)`) | collect |
| 22 | `collect.Decimal` | `toFixedScale` | `Decimal.toFixedScale`, returning `EitherNec[Failure, FixedScaleDecimal]` | collect |
| 23 | `collect.Decimal` | `equals`, `hashCode` | Value equality and hashing over the unscaled value and scale, exposed as `Hash[Decimal]` | collect |
| 24 | `collect.FixedScaleDecimal` | `of` | `FixedScaleDecimal.of`, returning `EitherNec[Failure, FixedScaleDecimal]` | collect |
| 25 | `collect.FixedScaleDecimal` | `decimal` | The `decimal` accessor of the case class, beside `fixedScale` | collect |
| 26 | `collect.Guavate` | `stream` | `.iterator`, `Option#iterator`, `LazyList` | — |
| 27 | `collect.Guavate` | `toImmutableSortedMap` | `Collections.toSortedMap` (duplicate keys reported as a `Failure`) or `SortedMap` directly | collect |
| 28 | `collect.Guavate` | `toImmutableSortedSet` | `scala.collection.immutable.SortedSet` | — |
| 29 | `collect.Guavate.ensureOnlyOne` | statically imported | `Collections.ensureOnlyOne`, returning `Either[Failure, Option[A]]` | collect |
| 30 | `collect.Guavate.filteringOptional` | statically imported | `Iterator#collect` / `List#flatMap` over `Option` | — |
| 31 | `collect.Guavate.inOptional` | statically imported | `Option#exists` / `Option#toList` | — |
| 32 | `collect.Guavate.list` | statically imported | `List(...)` | — |
| 33 | `collect.Guavate.toImmutableList` | statically imported | `scala.collection.immutable.List` (`.toList`) | — |
| 34 | `collect.Guavate.toImmutableSet` | statically imported | `scala.collection.immutable.Set` (`.toSet`) | — |
| 35 | `collect.Guavate.tryCatchToOptional` | statically imported | `scala.util.Try(...).toOption` | — |
| 36 | `collect.MapStream` | `of` | The `scala.collection.immutable.Map` itself — there is no stream stage to open | — |
| 37 | `collect.MapStream` | `map` | `Map#map` | — |
| 38 | `collect.MapStream` | `mapValues` | `Map#view.mapValues(...).toMap` | — |
| 39 | `collect.MapStream` | `filterKeys` | `Map#filter` on the key | — |
| 40 | `collect.MapStream` | `forEach` | `Map#foreach` | — |
| 41 | `collect.MapStream` | `toMap` | `.toMap`, `Collections.toSortedMap` where duplicate keys must be reported as a `Failure`, or `Collections.groupByPreservingOrder` where insertion order matters | collect |
| 42 | `collect.Messages` | `format` | Scala string interpolation (`s"..."`) | — |
| 43 | `collect.array.DoubleArray` | `of` | `DoubleArray.of(values: Double*)` — one varargs member, copying its input, in place of the Java type's ten arity-specific `of` overloads (no values, one through eight values, and eight values followed by a varargs tail), which existed only to spare a caller an array allocation per call. A collection is `DoubleArray.copyOf(Iterable[Double])` (`DoubleArray.scala:1114`); `of` takes no `Iterable` | collect |
| 44 | `collect.array.DoubleArray` | `filled` | `DoubleArray.filled` (size, and size with a fill value) | collect |
| 45 | `collect.array.DoubleArray` | `get` | `DoubleArray.get` — indexes the backing array directly, as Java did, so an out-of-range index raises `ArrayIndexOutOfBoundsException` (divergence (c)-11) | collect |
| 46 | `collect.array.DoubleArray` | `size` | `DoubleArray.size` (with `isEmpty`) | collect |
| 47 | `collect.array.DoubleArray` | `plus`, `minus`, `multipliedBy` | The same three members | collect |
| 48 | `collect.array.DoubleArray` | `stream` | `DoubleArray.iterator`, `toList` and `forEach` — no `DoubleStream` crosses the public API (Rule 10) | collect |
| 49 | `collect.array.DoubleArray` | `ofUnsafe`, `toArrayUnsafe` | No target: neither name exists in the port, so no caller anywhere can adopt or reach a backing array. Callers build with `of`, `copyOf`, `filled`, `tabulate`, `map`, `mapWithIndex` or `combine` and read with the copying `toArray` — see divergence (c)-13 | collect |
| 50 | `collect.array.DoubleArray` | `DoubleArray.class` as a Joda-Beans meta-property type literal (`CurrencyAmountArray`) | No target — there are no meta-beans; the Scala type appears directly in the field's type | — |
| 51 | `collect.array.DoubleMatrix` | `get` | `DoubleMatrix.get(row, column)` — direct indexing, as Java did (divergence (c)-11) | collect |
| 52 | `collect.array.DoubleMatrix` | `toArray` | `DoubleMatrix.toArray` — deep-copies every row | collect |
| 53 | `collect.array.DoubleMatrix` | `ofUnsafe`, `toArrayUnsafe` | No target: neither name exists in the port. Callers use `of`/`ofArrays`/`ofArrayObjects`/`copyOf`/`tabulate` and read with the copying `toArray`, `rowArray` and `columnArray` — see divergence (c)-13 | collect |
| 54 | `collect.array.DoubleMatrix` | `DoubleMatrix.class` as a Joda-Beans meta-property type literal (`FxMatrix`) | No target — there are no meta-beans | — |
| 55 | `collect.io.CsvFile` | `of`, `rows` | No target — the index CSV tables are Scala data objects (`IborIndexData`, `OvernightIndexData`, `PriceIndexData`, `FxIndexData`) | — |
| 56 | `collect.io.CsvRow` | `getValue`, `getField` | No target — a data-object row is a typed Scala value, so a column is read by name at compile time | — |
| 57 | `collect.io.IniFile` | `of`, `section`, `sections`, `asMap` | No target — the INI tables are Scala data objects, and the alias, external and lenient tables live in their families' companions | — |
| 58 | `collect.io.PropertiesFile` | `of`, `getProperties` | No target — `country.properties` became `location.CountryData` | — |
| 59 | `collect.io.PropertySet` | `keys`, `value`, `asMap` | No target — same reason as `IniFile` | — |
| 60 | `collect.io.ResourceConfig` | `combinedIniFile`, `orderedResources` | No target — the `base`/`library`/`application` override chain is deliberately not ported | — |
| 61 | `collect.io.ResourceLocator` | `ofClasspath`, then `getCharSource` | `io.Resources.readClasspathText(path): IO[String]`, whose callers are all in test scope: the parity fixtures, the reference-data manifest and `ResourcesSpec` | collect |
| 62 | `collect.io.ResourceLocator` | `getByteSource`, then `io.BeanByteSource.openStream` — `GlobalHolidayCalendarLookup` reading `GlobalHolidayCalendars.bin` | No target — the generated calendar cache is not carried and the generators are pure functions (divergence (c)-31) | — |
| 63 | `collect.io.ResourceLocator` | `getCharSource`, then `io.BeanCharSource.read` — `ImmutableHolidayCalendarTest` reading `ImmutableHolidayCalendar-Old.json` | No target for the reader: Joda wire compatibility is out of scope (divergence (c)-30), and the fixture is not carried. The test **method** is ported rather than dropped — `java-test-mapping.csv` maps it `ported` to `ImmutableHolidayCalendarSpec`'s `test_readOldJodaFormat`, which asserts the *refusal*: the legacy document is built in the spec as text, with and without the `@bean` key, and the decoder rejects both with a message naming the `Immutable` wrapper it requires, while the same calendar in this port's own form decodes with the dates it declares | — |
| 64 | `collect.named.Named` | `getName`, implemented by every named `strata-basics` type | `Named.name` (universal trait, `extends Any`) | collect |
| 65 | `collect.named.NamedEnum` | the marker interface implemented by the three Java `enum` families — `FloatingRateType`, `StubConvention`, `ValueAdjustmentType` | A `NamedEnum[A]` instance in each sealed family's companion | collect |
| 66 | `collect.named.NamedLookup` | `lookupAll`, implemented by the nine basics providers — the four `*CsvLookup`s, `FloatingRateNameIniLookup`, `HolidayCalendarIniLookup`, `GlobalHolidayCalendarLookup`, `DayRollConventions` and `Business252DayCount` | No target — there is no provider mechanism to implement; a family's instances exist only in its companion, and the day-roll and `Bus/252` families build theirs there | — |
| 67 | `collect.named.ExtendedEnum` | `of` (registry construction) | `NamedEnum.of(values, alternates, lenient, externals)` — an in-code table, no classpath scan | collect |
| 68 | `collect.named.ExtendedEnum` | `lookup`, `find` | `NamedEnum.valueOf(name): Option[A]` (alias-aware); where Java's `lookup` threw, the caller returns its own `Either` | collect |
| 69 | `collect.named.ExtendedEnum` | `findLenient` | `NamedEnum.parse(name): EitherNec[Failure, A]` (alias, then the ordered lenient rewrites, then alias again) | collect |
| 70 | `collect.named.ExtendedEnum` | `lookupAll`, `lookupAllNormalized` | `NamedEnum.byUpperName`, `NamedEnum.byCanonicalName` | collect |
| 71 | `collect.named.EnumNames` | `of` | `NamedEnum.of` with `NamedEnum.showByName` / `orderByName` / `hashByName` | collect |
| 72 | `collect.named.EnumNames` | `format` | The value's own `name`, exposed as `Show[A]` | collect |
| 73 | `collect.named.EnumNames` | `parse` | `NamedEnum.parse` | collect |
| 74 | `collect.named.CombinedExtendedEnum` | `of` | One `parse`/`tryParse` per union: `Index.parse`, `RateIndex.parse`, `FloatingRateIndex.parse`, `FloatingRate.parse`, each trying its families in the Java order | basics |
| 75 | `collect.named.CombinedExtendedEnum` | `lookup` | The same union `parse`, which answers the first family that resolves the name | basics |
| 76 | `collect.tuple.Pair` | `of`, `getFirst`, `getSecond` | `Tuple2` (`(a, b)`, `._1`, `._2`) | — |
| 77 | `collect.TestHelper` | `date` | `testkit.TestHelper.date` | collect (test) |
| 78 | `collect.TestHelper` | `list` | `testkit.TestHelper.list` | collect (test) |
| 79 | `collect.TestHelper` | `caputureLog` (Java spelling) | `testkit.TestHelper.captureLog` (and `captureStdOut`, `snapshot`) | collect (test) |
| 80 | `collect.TestHelper` | `coverImmutableBean`, `coverBeanEquals`, `coverEnum`, `coverPrivateConstructor` | No target — reflective bean sweeps are replaced by ScalaCheck equality, `Show` and round-trip properties | — |
| 81 | `collect.TestHelper` | `assertSerialization` | No target — replaced by the circe round-trip properties of section (e) | — |
| 82 | `collect.TestHelper` | `assertJodaConvert` | No target — Joda-Convert is gone; the string form is asserted through `Show` and the string codecs | — |
| 83 | `collect.TestHelper` | `assertUtilityClass` | No target — a Scala `object` has no constructor to hide | — |
| 84 | `collect.CollectProjectAssertions` | the class itself, extended by `BasicProjectAssertions` for `assertThat(Result)` / `assertThat(ValueWithFailures)` | `testkit.ResultMatchers` — `beSuccess`, `beFailure`, `haveValue`, `beFailureWith`, `value`, `failures`, `haveFailureMessageMatching`, mixed into a spec rather than inherited from an assertions base class | collect (test) |
| 85 | `collect.Unchecked` | `wrap` | `scala.util.Try` / `scala.util.Using`, or a plain expression where nothing was checked | — |

### Supplemental mappings — outside the required inventory

The rows below are **not** part of the AAP's required inventory: none of them is a `strata-collect`
member that Java `strata-basics` imports or references. They are recorded because the Scala port
depends on them and a reader looking for them would otherwise find nothing. **11 rows.**

| # | Symbol | Referenced by Java `strata-basics` | Scala replacement | Module |
|---|--------|------------------------------------|-------------------|--------|
| S1 | `collect.result.Result` — `of`, `success`, `failure`, `map`, `flatMap`, `combine`, `sequence`, `allSuccessful`, `anyFailures`, `countFailures` | No | `FailureOr[A] = Either[Failure, A]` and `ResultNec[A] = EitherNec[Failure, A]` with cats syntax, plus `result.combine`, `result.sequence`, `result.flatCombine`, `result.allSuccessful`, `result.anyFailures`, `result.countFailures` | collect |
| S2 | `collect.result.FailureItem` — `of`, `getReason`, `getMessage`, `getAttributes` | No | The sealed `Failure` ADT: one case class per reason, each with `message` and `attributes: SortedMap[String, String]` | collect |
| S3 | `collect.result.FailureReason` — the ten reasons | No | The sealed `FailureReason` family with a `NamedEnum` instance | collect |
| S4 | `collect.result.ValueWithFailures` — `of`, `withAdditionalFailures`, `combinedWith` | No | `ValueWithFailures[A] = Ior[NonEmptyChain[Failure], A]` plus the `result.ValueWithFailures.*` helpers | collect |
| S5 | `collect.TypedString` — the validated string base class | No | `com.opengamma.strata.collect.TypedStringCompanion[T]`, a top-level class in `TypedString.scala`, over a `final class ... extends AnyVal with Named`. No `strata-basics` type is a typed string today | collect |
| S6 | `collect.DoubleArrayMath` — the array helpers | No | `DoubleArrayMath`, `var`-free. The **pure subset** is ported: `toObject`, `toPrimitive`, `sum`, `apply`, `applyAddition`, `applyMultiplication`, `combine`, `combineLenient`, `combineByAddition`, `combineByMultiplication`, `fuzzyEquals`, `fuzzyEqualsZero`, `isMathematicalInteger`, `reorderedCopy`, `sortPairs`. Java's in-place `mutate`, `mutateByAddition` (two overloads) and `mutateByMultiplication` (two overloads) are deliberately not ported — an immutable array API has nothing for them to write to (Rule 3) | collect |
| S7 | `collect.named.ExtendedEnum.externalNames` | No | `NamedEnum.externalNames(group)`, `externalNamesRaw(group)`, `externalNameGroups` — kept because the FpML and SWIFT alias groups belong to the collect contract | collect |
| S8 | `collect.io.ResourceLocator.ofFile` | No | `io.Resources.readFileText(path): IO[String]` — see divergence (c)-16 | collect |
| S9 | Guava `DoubleMath.fuzzyEquals` and `DoubleMath.isMathematicalInteger` — used by Java `strata-basics` directly rather than through `strata-collect` (the fixed-rate `convertedTo` overloads of `CurrencyAmount`/`Money`/`BigMoney`; `CurrencyAmount.toString` and `FxRate.toString`) | Yes, but as Guava, not as `strata-collect` | `DoubleArrayMath.fuzzyEquals` (scalar and array forms) and `DoubleArrayMath.isMathematicalInteger`. The NaN and infinite-tolerance behaviour of `fuzzyEquals` follows the AAP rather than Guava — see (c)-9 | collect |
| S10 | `collect.Guavate.only(Iterable)` and `Guavate.toOnly()` — the "exactly one element" pair, both answering an empty `Optional` | No | `Collections.ensureOnlyOne`, returning `Either[Failure, Option[A]]`, which **distinguishes** the two cases the Java pair conflated: no element is `Right(None)`, one element is `Right(Some(value))`, and several elements are a `Failure`. The default form names the first two elements it found (`Multiple values found where only one was expected: <first> and <second>`) and stops there; the overload that takes a message by name carries the caller's text instead and never reads those elements into it. A caller that wanted "several" treated as "none" writes `.toOption.flatten` | collect |
| S11 | `collect.result.Result.of(Supplier)`, `Result.wrap(Supplier)`, `ValueWithFailures.of(value, Supplier)` — the factories that ran a caller's block and turned what it threw into a failure — and `Result.mapFailureItems` | No | No target, and none is supplied. Nothing in either module raises in order to report a failure, so a computation that can fail returns its failures and there is no block to capture; a caller holding Java code that throws wraps it itself with `scala.util.Try`. `mapFailureItems` has no subject either — there is no item type, so both of Java's failure-mapping members arrive at `leftMap` over the sealed `Failure` ADT | collect |

Guava's `Immutable*` collections become `scala.collection.immutable`, its `Splitter`/`Joiner` become
`String#split`/`mkString`, and `Suppliers.memoize` becomes a `lazy val`. Joda-Beans and Joda-Convert
have no replacement member: the bean machinery becomes ordinary Scala values with circe codecs, and
`@ToString`/`@FromString` become `Show` and `NamedEnum.parse`.

### Published members the port adds — no Java counterpart

Also outside the AAP's required inventory, and in the other direction: the rows below replace no
Java member. They are members this port **publishes** that the Java modules do not have — three in
`strata-basics` and five in `strata-collect` — so a reader comparing the two surfaces finds them in
neither table above. Each is a constant naming a ceiling, each is read by the divergence row beside
it, and each is public because a caller that has to stay inside a ceiling has to be able to read the
figure rather than infer it from a failure. **8 rows.**

| # | Published member | Value | Why it is published | Module | Divergence |
|---|------------------|-------|---------------------|--------|------------|
| P1 | `date.HolidayCalendar.MaxBusinessDayShift` | 100 000 business days | The largest shift any calendar walks, and the figure `DaysAdjustment` judges a business-day count against when it is built. A caller assembling an adjustment from data reads it to know what will be accepted | basics | (c)-47 |
| P2 | `date.HolidayCalendar.MaxConsecutiveHolidays` | 3 653 days | The furthest a search for a business day walks before deciding the calendar has none | basics | (c)-47 |
| P3 | `date.HolidayCalendar.MaxCompositeDepth` | 128 calendars | The tallest composite that `combinedWith`, `linkedWith`, the decoder and `HolidayCalendarId.resolve` build | basics | (c)-48 |
| P4 | `json.Codecs.MaximumArrayElements` | 1 048 576 | The longest run of doubles a document may ask a decode to allocate | collect | row 29 |
| P5 | `json.Codecs.MaximumMatrixRows` | 4 096 | The most rows a document may state for a matrix | collect | row 29 |
| P6 | `json.Codecs.MaximumMatrixColumns` | 4 096 | The most elements a document may state in one matrix row | collect | row 29 |
| P7 | `json.Codecs.MaximumMatrixElements` | 1 048 576 | The most elements across every row, checked as a 64-bit product so a stated shape cannot wrap past it | collect | row 29 |
| P8 | `json.Codecs.MaximumCollectionElements` | 100 000 | The most elements a document may state for a bounded collection field of a value | collect | row 29 |

The ceilings that are **not** published are as deliberate as these: `Resources.MaxBytes` and
`ReadTimeLimit`, the composite-name ceilings of `HolidayCalendarId`, the parse-text ceilings of
section (c) row 49 and the diagnostic bounds of row 51 are all private to the object that applies
them, because a caller has nothing to choose about any of them and meets each only as the failure
that names it.

## (b) Collect symbols deliberately not ported

`strata-collect` was ported as the *subset* `strata-basics` uses, so that the module stays small,
reusable and free of anything `strata-basics`-specific. Everything below exists in the Java module and
has no Scala counterpart. A later slice that needs one ports it then, from the Java source, which is
still in the repository.

| Java symbol or package | Why it is not ported |
|---|---|
| `collect.function.*` (the primitive functional interfaces) | Scala function types and, where a primitive callback is needed to avoid boxing, a single-abstract-method trait declared beside its user — see divergence (c)-15 |
| `collect.timeseries.*` | No `strata-basics` type uses a time series |
| `collect.concurrent.*` | The port has no concurrency surface; the core is pure |
| `collect.io.*` except a text reader | The INI/CSV/properties readers served the runtime configuration mechanism this port replaces with Scala data. Only the classpath/file text reader survives, as `io.Resources`; its callers are the test scopes — the parity fixtures, the reference-data manifest and `ResourcesSpec` — and no main source of either module uses it |
| `collect.array.IntArray`, `collect.array.LongArray` | Unused by `strata-basics`; only the `double` array and matrix are needed |
| `collect.BasisPoints`, `collect.Percentage` | Unused by `strata-basics` |
| `collect.NumberFormatter` | Unused; `Decimal.format`/`formatAtLeast` cover the ported formatting |
| `collect.CharMatchers` | Unused; character predicates are ordinary Scala functions |
| `collect.Version` | Build metadata, not behaviour |
| `collect.tuple.Triple`, the `collect.tuple.Tuple` trait itself, and the primitive pair types (`IntDoublePair`, `LongDoublePair`, `ObjDoublePair`, `ObjIntPair`, `DoublesPair`) | `Tuple2`/`Tuple3` replace them, and a Scala tuple needs no common supertype; only `Pair` was used and it maps to `Tuple2` |
| `collect.result.FailureException`, `FailureItemException`, `IllegalArgFailureException`, `ParseFailureException`, and `collect.UncheckedReflectiveOperationException` with the `Unchecked` wrappers it served | **No exception type is introduced in `strata-collect`.** A data failure is an `Either` value; escalation to a thrown error happens only at the `IO` edges (the demo and the parity harness) through `IO.fromEither` |
| `collect.named.NamedLookup`, `ExtendedEnum` providers, `CombinedExtendedEnum` | The runtime registry is replaced by closed sealed families and a `NamedEnum` typeclass — see divergence (c)-5 |
| `collect.io.ResourceConfig`'s `base`/`library`/`application` override chain | Runtime extended-enum extensibility is deliberately dropped |
| The `[providers]` section of every INI file that has one — `BusinessDayConvention.ini`, `DateSequence.ini`, `DayCount.ini`, `FloatingRateName.ini`, `FxIndex.ini`, `HolidayCalendar.ini`, `IborIndex.ini`, `OvernightIndex.ini`, `PeriodAdditionConvention.ini`, `PriceIndex.ini`, `RollConvention.ini` | A `[providers]` row named the class that supplied a family's instances at run time, which is the mechanism this port removes: the instances now exist only in the family's own companion, so there is no provider to name. The `[alternates]`, `[externals.*]` and `[lenientPatterns]` sections of those same files **are** ported, as Scala data — see divergence (c)-5 |
| The `[types]` section of `Index.ini`, `RateIndex.ini` and `FloatingRateIndex.ini` | Each declared one `CombinedExtendedEnum` union over other families. Each becomes a `parse`/`tryParse` in the corresponding Scala companion, trying the same families in the same order (section (a), rows 74 and 75) |
| `HolidayCalendarIniLookup`, `IborIndexCsvLookup`, `OvernightIndexCsvLookup`, `PriceIndexCsvLookup`, `FxIndexCsvLookup`, `FloatingRateNameIniLookup`, `CurrencyDataLoader`, `GlobalHolidayCalendarLookup` (in `strata-basics`) | Their whole responsibility is met by the `*Data.scala` objects and `NamedEnum`; there is nothing left to load |
| `ImmutableHolidayCalendarDeserializer`, `META-INF/org/joda/beans/JodaBeans.ini`, `GlobalHolidayCalendars.bin` | Joda-Beans serialization and the generated calendar cache are not carried — see divergence (c)-31 |
| `java.io.Serializable` / `Externalizable` support on any type | **Removed, not replaced.** No type of either module implements either interface. A codec-bearing data type serializes through circe instead (section (e)); a type that section (e) excludes has no replacement serialization at all — Java `CalculationTargetList` was `Serializable`, and its Scala counterpart, being a list of contract objects with no data of their own, has no codec |
| A Java-callable façade | Excluded by design decision D-3 — see divergence (c)-4 |
| Java test artefacts exercising removed machinery: `HolidayCalendarIniLookupTest`, `FailureExceptionTest`, `FailureItemExceptionTest`, `IllegalArgFailureExceptionTest`, `ParseFailureExceptionTest`, the `HolidayCalendarData*.ini` test fixtures, `ImmutableHolidayCalendar-Old.json` | Their subjects are gone. The method-level record of every dropped, consolidated and ported Java test is `strata-basics/src/test/resources/manifest/java-test-mapping.csv` |

Of the `Guavate` and `MapStream` surfaces, only the members listed in section (a) are ported;
`Collections.scala` holds `ensureOnlyOne`, `toSortedMap`, `groupByPreservingOrder` and the
`NonEmptyChain` helpers, and nothing else.

## (c) Deliberate divergences from the Java original

Each row is a behaviour that is **not** identical to Java `2.12.74-SNAPSHOT`, deliberately — row 9
among them, where the AAP directs the port away from the Guava comparison the Java code called.
Exactly three rows have no Java behaviour to quote, and they are the three whose Java-behaviour cell
reads `—`: row 19 on the parity harness, which is tooling of the port rather than a ported member;
row 32 on the test sources and the manifest; and row 46 on the shape of `IndexObservation`, which
Java's own interface shares. Those three are structural rather than behavioural. Every row describes
code that is in the tree; row numbers are referenced from sections (a), (d), (e) and (f), so they are
stable and are never reused.

**What supports a row.** The rows differ in the kind of evidence behind them, and the difference
matters when one is checked:

- *Committed two-sided measurements.* Numeric and calendar behaviour is pinned by Java baselines
  captured from the `2.12.74-SNAPSHOT` jars and committed with the port, each read by the spec beside
  it: `parity/daycount-baseline.json` (`DayCountParitySpec`), `schedule-baseline.json`
  (`ScheduleParitySpec`), `fx-baseline.json` (`FxParitySpec`), `currency-math-baseline.json`
  (`CurrencyMathParitySpec`), `holiday-baseline.json` (`HolidayCalendarParitySpec`),
  `strata-collect/.../parity/double-array-baseline.json` (`DoubleArrayParitySpec`); the reference-data
  tables by `manifest/reference-data-manifest.json` (`ReferenceDataManifestSpec`).
- *Committed port-side assertions.* A port value quoted in a row is what this build answers and is
  asserted by the suite — `DoubleArrayMathSpec`, `DoubleArraySpec`, `DoubleMatrixSpec`,
  `FrequencySpec`, `HolidayCalendarIdSpec`, `SmartConstructorSpec`, `FailableSurfaceSpec`,
  `JsonRoundTripSpec` and the family specs of the type named in the row.
- *Reproducible but not committed.* A Java value quoted in a row and not carried by one of those
  fixtures was produced ad hoc by driving the Maven-built jars through `jshell`, by the procedure in
  [`tools/parity-capture/README.md`](tools/parity-capture/README.md). It is reproducible by that
  command; it is not an artifact in this tree, and nothing in the suite re-checks it.
- *Structural.* Rows about API shape, privacy, types and signatures are established by reading the
  member the row names; several are additionally proved at compile time by `ApiSurfaceSpec`.

| # | Area | Java behaviour | Port behaviour | Why |
|---|------|----------------|----------------|-----|
| 1 | Error model | A data failure throws — `IllegalArgumentException`, `IllegalStateException`, `ScheduleException`, `ReferenceDataNotFoundException` | The same failure is returned: `Either[Failure, A]`, or `EitherNec[Failure, A]` where several causes accumulate, over a sealed `Failure` ADT with ten reasons. `ScheduleException` becomes `Failure.Invalid` carrying a `definition` attribute; FX failures become `Failure.CurrencyConversion`; a missing calendar becomes `Failure.MissingData` | Explicit, total error handling (Rule 5). A failure that depends on argument *data* belongs in the signature |
| 2 | Retained throws | Every precondition throws | A caller-contract or numeric-edge precondition still throws `IllegalArgumentException`, through `ArgCheck`, and is documented on the member: array and matrix **dimension** preconditions (a negative or mismatched size, a sub-array bound, a reduction over an empty array); a calendar query outside years 0–9999; a NaN produced by arithmetic on infinite `CurrencyAmount` operands; `Decimal` overflow past 18 digits; `DayCount.yearFraction` with dates out of order or without the schedule information the day count reads. Three further throws are the port's own rather than Java's, and each is a refusal Java did not make: a value that is not a number reaching the construction point of `CurrencyAmountArray` or `MultiCurrencyAmountArray` through one of their members that is total in signature — `CurrencyAmountArray.of(currency, DoubleArray)`, and the `multipliedBy` and `mapAmounts` of each, where a factor of zero applied to an infinite element, a factor that is itself not a number, or a mapper that produces one is what reaches it (row 52); a business-day shift or `DaysAdjustment` naming more than `HolidayCalendar.MaxBusinessDayShift` days through a member that is total in signature; and a search for a business day that crosses `HolidayCalendar.MaxConsecutiveHolidays` closed days without finding one (row 47). Element **access** is not among them — `DoubleArray.get` and `DoubleMatrix.get` index the backing array directly on both sides, so an out-of-range index is an `ArrayIndexOutOfBoundsException`, not an `ArgCheck` failure (row 11) | A contract violation is a programming error, not a data outcome; putting it in the signature would tax every correct caller |
| 3 | Null arguments | Rejected with `IllegalArgumentException` from `ArgChecker.notNull` (measured: `StandardId.of(null, "v")` → `Argument 'scheme' must not be null`) | **Outside the contract of every public entry point.** `null` is not a value the port accepts, guards or documents; passing one raises `NullPointerException` where the argument is dereferenced, except `Currency.of(null)`, which happens to answer `Left(Failure.Parsing("Currency name not found: null"))` because it is a table lookup. See (c)-3 below | The `ArgChecker.notNull` family is deliberately not ported, and Scala code has `Option` for absence. Guarding `null` in Scala signatures would pay for a Java hazard the port has no Java callers to protect |
| 4 | Public API shape | Java types: `Optional`, `java.util` collections, Guava `Immutable*`, checked names | Scala-native throughout: `Option`, `scala.collection.immutable`, `cats.data.{NonEmptyList, NonEmptyChain, Validated, Ior, Kleisli}`, cats typeclass instances. No Java-callable façade and no interop shim | Design decision D-3. A façade would constrain every signature to what Java can express |
| 5 | Named families | `ExtendedEnum` reads INI files off the classpath at class-initialisation time; applications extend a family by adding a provider or an INI override | Each family is a **closed** sealed type whose instances exist only in its companion, with a `NamedEnum[A]` instance. Runtime extensibility is gone. The *behaviour* the INI files encoded is kept as Scala data: alias tables, `[externals.FpML]`/`[externals.SWIFT]` groups and the ordered `[lenientPatterns]` rewrites, so `parse` resolves exactly the names Java resolved. One narrowing: the lenient stage of a family — the fold to upper case and the ordered rewrites — is applied only to text within `NamedEnum.lenientLengthCeiling`, which each family derives from its own data (its longest lookup key, its longest alternate spelling, its longest alternate target and its longest expression source, plus a margin of 32 characters, giving 35 for `Currency` and 52 for the sixty-seven-row `DayCount` family). Text beyond it is reported as not found, with the family's own message, rather than being folded and scanned. The bound is applied in all three places the lenient stage can be entered, and in each of them *before* the fold to upper case, which is itself a copy of the whole input: `NamedEnum.parse`, `NamedEnum.rewriteLeniently` (so a caller running the chain itself is bounded even if it folded first), and `DayCount.parseWith`, which runs stage two itself in order to interleave its `Bus/252` provider and therefore checks the ceiling before folding. The *exact* lookup is not bounded at all, which is why every name, alias, external and lenient row of every family is inside its ceiling and a `Bus/252` name naming thousands of calendars still resolves. One registration rule is applied to every family where Java had two, which matters only for names that differ in case alone - see (c)-5 below | Rule 4, and a closed family is exhaustively checkable by the compiler. `main` sources reference no `.ini`, `.csv` or `.properties` resource. The lenient bound makes refusing a name cost what the name costs: an unbounded miss was a full pass over the text per rewrite, which external input chooses the length of |
| 6 | `Currency` | Any three upper-case letters mint a currency, guessing zero minor units and USD triangulation (measured: `Currency.of("XYZ")` → `XYZ`, `minorUnitDigits=0`, triangulation `USD`) | The closed set of the 74 configured currencies. An unknown code is `Left(Failure.Parsing("Currency name not found: XYZ"))` (measured). `Country`, by contrast, keeps Java's open code space — any `[A-Z][A-Z]` is accepted (measured: `Country.of("ZZ")` → `Right(ZZ)`) | Rule 4 closes the currency family; AAP Conflict 4. A minted currency with guessed conventions is a silent data error, and `Country` is not a named-enum family in the first place |
| 7 | `FxIndex` | `FxIndex.of(pair)`/`of(name)` mint an index for an unconfigured pair through `createFxIndex`, using the pair's default calendar and a two-day maturity offset (measured: `FxIndex.of("GBP/SEK")` → an index) | The closed set of the 16 configured rows. `of(pair)` answers the configured index with the lowest name, as Java's `min` does, and `Left(Failure.Parsing)` for an unconfigured pair. `createFxIndex` is not ported | Rule 4; AAP Conflict 7 |
| 8 | `ReferenceDataNotFoundException` | Thrown by `ReferenceData.getValue` and by `resolve` | Not ported. `getValue(id)` is `Either[Failure, T]` with `Failure.MissingData` carrying the id as an attribute; `findValue(id)` is `Option[T]`; `containsValue(id)` is unchanged | Row 1, applied to reference data. The exception type had no other use |
| 9 | Tolerance comparison at NaN and at an infinite tolerance | Guava's algorithm is `copySign(a - b, 1.0) <= tolerance \|\| a == b \|\| (isNaN(a) && isNaN(b))`; its third clause makes two NaNs fuzzy-equal (measured: `DoubleMath.fuzzyEquals(NaN, NaN, 0.1)` is `true`), and its magnitude test equalises any two non-NaN values at an infinite tolerance | The comparison classifies before it measures: a NaN on either side is equal to nothing, an infinity is equal only to the same infinity whatever the tolerance, and two finite values are equal when their distance does not exceed it (`DoubleArrayMath.scala:568`). Measured: `fuzzyEquals(NaN, NaN, 0.1)` is `false` at every tolerance, `fuzzyEquals(+Inf, +Inf, 0.0)` is `true`, and `fuzzyEquals(+Inf, -Inf, +Inf)` and `fuzzyEquals(0.0, +Inf, +Inf)` are `false` | AAP §0.3.3 requires Guava's semantics **with `NaN` never fuzzy-equal and each infinity fuzzy-equal only to itself**, and §0.4.2 restates it. The AAP is frozen and authoritative, so the two edges where Guava disagrees with it follow the AAP; (c)-9 states the rule, the shape of the comparison and what each entry point answers |
| 10 | Tolerance argument | Guava rejects a NaN tolerance (`tolerance (NaN) must be >= 0`) | Rejected too, through `ArgCheck.notNaN`, with the port's message `Argument 'tolerance' must not be NaN` (measured). The private near-zero test in `ArgCheck`/`Validate` keeps the Java reading — `abs(x) <= tolerance \|\| x == 0.0` — while `DoubleArrayMath`'s zero comparison follows the AAP rule of row 9, so the two answer the same on every input a caller is likely to pass and differ at an *infinite* tolerance over an infinity (measured: `ArgCheck.notZero(+Inf, +Inf, "x")` throws `Argument 'x' must not be zero`, whereas `fuzzyEqualsZero([+Inf], +Inf)` is `false`). A NaN counts as near zero on neither (measured: `ArgCheck.notZero(NaN, 1e-9, "x")` returns and `fuzzyEqualsZero([NaN], 1e-9)` is `false`) | The message set is the port's own, and the Java check is the authority for these two checks and their messages, which is why the local test keeps its reading; the two are deliberately kept in separate files rather than one calling the other, because that object checks its own tolerance through this one and calling back would tie the two into a cycle |
| 11 | Numeric precondition types — **checked preconditions only** | `DoubleArray.EMPTY.min`/`max` → `IllegalStateException`; `subArray(4)` on a 3-element array → `IndexOutOfBoundsException`; `DoubleMatrix.identity(-1)` → `NegativeArraySizeException`; `DoubleMatrix.filled(0, -1)` → the empty matrix (all measured) | Every one of those becomes an `IllegalArgumentException` through `ArgCheck`, with Java's message text preserved where Java had one (`Unable to find minimum of an empty array`, `Array index out of bounds: 4 > 3`) and the argument named where it did not (`Argument 'size' must not be negative but has value -1`); `filled(0, -1)` and `of(0, -1)` now fail rather than returning the empty matrix. A negative length rejects the call rather than producing a value, with one exception type and one message shape across all four array and matrix size paths: `DoubleArray.filled(-1)`, `DoubleArray.filled(-1, 2.0)` and `DoubleArray.tabulate(-1)` check through `ArgCheck.notNegative` before allocating and report `Argument 'size' must not be negative but has value -1`, matching `DoubleMatrix.identity(-1)` exactly (all measured), and `DoubleMatrix.filled(0, -1)` names its own argument (`Argument 'columns' …`); no port path lets the allocator's `NegativeArraySizeException` escape. **Element access is outside this row and is unchanged:** `DoubleArray.get` and `DoubleMatrix.get` index the backing array directly, with no `ArgCheck` call, so `get(-1)` raises `ArrayIndexOutOfBoundsException` on both sides | AAP §0.3.3 classes array and matrix dimension errors as fail-fast `ArgCheck` invariants, and checking before allocating is what stops a negative or huge dimension reaching the allocator. Element access is left to the JVM's own bounds check, which is the same behaviour Java had and costs the hot path nothing. **A caller must not discriminate on the exception type of a checked precondition failure across this boundary** |
| 12 | Ragged matrix input | `DoubleMatrix.copyOf` accepts a ragged `double[][]` and returns a value whose `total()` and `get(row, col)` then throw `ArrayIndexOutOfBoundsException`, while `toString` renders each row at its own length (all measured) | A matrix is rectangular by contract: `ofArrays` rejects a wrong-length row (`Function returned array of incorrect length 1, expected 2`) and the JSON decoder refuses a ragged payload (`Expected every row of the matrix to hold the same number of elements`). Ragged input has no defined matrix meaning, and Java's permissiveness here — which produced a value whose own reads throw — is not preserved as a feature. `copyOf` rejects it as well, and so does every other route, because the rows are measured in the one constructor they all pass through, before anything is cloned — `copyOf([[1,2],[1]])` reports `Expected every row of the matrix to hold 2 elements, but row 1 holds 1` and `copyOf([[1],[1,2]])` names 1 element against row 1's 2 (both measured) — where Java copied the array and handed back a value whose own `total`/`get` then threw. No value of this type is non-rectangular, whatever route built it, so `toString` renders a rectangle and rendering stays total, as Java's did | A `[T]` total type with copy-safe factories should not be able to produce a value whose own reads fail, and the three factories plus the JSON decoder now answer raggedness the same way |
| 13 | Array aliasing | `DoubleArray.ofUnsafe`/`toArrayUnsafe` and `DoubleMatrix.ofUnsafe`/`toArrayUnsafe` are public, and alias the caller's array | Neither member is ported, and immutability is a property of the compiled form rather than of a visibility. A `private[collect]` member is emitted as a public method, so restricting these in the source would have left the same two aliasing entry points in the bytecode; instead they are gone, no member of either type answers with the storage it holds, and the sole constructor of each — which the compiler must emit publicly, because the companion constructs through it — **copies** what it is handed (`DoubleArray` clones the array, `DoubleMatrix` deep-clones the rows after measuring them). Every public factory and accessor therefore copies: `of`, `copyOf`, `filled`, `tabulate`, `ofArrays`, `ofArrayObjects`, `toArray`, `rowArray`, `columnArray`. `strata-basics` builds arrays with `tabulate`, `map`, `mapWithIndex` and `combine`. Measured through Java reflection: constructing either type from a caller's array and then mutating that array leaves the value unchanged, and `toArray` answers with a distinct array on every call. An operation that produces a run of values pays exactly one allocation and one bulk copy, because the copy the constructor makes *is* the result buffer: the constructor takes the values together with a sealed, `private[array]` description of the operation it is constructing for, produces the storage it will keep — a clone of those values, a range copy of them, or the deep copy of the rows — and applies that operation's own loop to that storage, in ascending index order or row-major order, before the value is published; cloning the source and rewriting the clone in place holds exactly what filling a fresh buffer from the source would have held, because the clone begins as a copy of the source. The dispatch is one type test per value constructed rather than a virtual call per element, so the ten specialised monomorphic loop helpers of `DoubleArray` survive as in-place variants, `DoubleMatrix` gains six of the same shape, and no call site became megamorphic. Measured from the bytecode with `javap -c -p`: thirteen `DoubleArray` operation bodies went from one array-allocating instruction to none and `DoubleMatrix.with` from two to none, the element-wise matrix operations no longer reach `tabulate`'s rectangle at all, and each constructor holds its allocations behind mutually exclusive branches — fourteen for the array, eight for the matrix — so exactly one runs per constructed value. No adopting member was added and none is needed: a companion-private member reached from the class is emitted as a public method as well — the Rule 3 gate's allow-list has to name `com$opengamma$strata$collect$array$DoubleMatrix$$deepClone` for exactly that reason — so a member that adopted a freshly filled run would have been the same aliasing entry point under another name, whereas a constructor that produces its own storage on every branch cannot keep what it is handed, whatever language the caller was compiled from, and the description it takes carries a scalar, an index, a callback or another value of the type, never an array to hand out (asserted reflectively over every rewrite in `DoubleArraySpec` and `DoubleMatrixSpec`, together with the closed family and the copy made through each route). Construction that does not start from a run of values keeps a buffer of its own and still pays the extra copy — `of`, `copyOf` of a collection, `filled`, `tabulate`, `concat`, and the matrix's `ofArrays`, `ofArrayObjects`, `identity`, `diagonal` and `transpose` — because none of those has a run of values for the constructor to derive storage from. Both types state this in their class-level implementation notes | Rule 3 immutability. The cost is one array copy per constructed value, which is paid to make the guarantee hold for every caller the bytecode admits — including a Java or package-spoofing caller, for which a source-level restriction does nothing. AAP §0.8.4 specifies no numeric performance figure and no benchmark is committed with the port, so that copy is not weighed against one; it is the price of the guarantee |
| 14 | `DoubleArrayMath.sortPairs` | An in-place recursive dual-array quicksort (`dualArrayQuickSort`) that **mutates the caller's arrays**, is not stable, and is `O(n²)` in the worst case | Pure: it returns fresh arrays and leaves its arguments untouched, sorting a stable bottom-up merge sort over one `Array[Int]` permutation plus one `Array[Int]` buffer that the passes alternate between rather than copying back over, comparing `java.lang.Double.compare` directly — stable, no boxing, and `O(n log n)` in the worst case, where the Java sort was `O(n²)`: the worst-case complexity class **improves**. The allocation is bounded and does not depend on the input's order: two result arrays plus one index buffer, and neither the buffer nor any merging for keys already in ascending order (an `O(n)` scan answers with the identity permutation, which is what a stable merge of such an input produces). The length-mismatch message is Java's (`Arrays cannot be sorted as they differ in length`) | Immutability (Rule 3) rules out sorting the caller's arrays, stability is what lets a value of any element type follow its key, and the permutation sort is what keeps the operation `var`-free and boxing-free. Returning fresh arrays costs allocation that an in-place sort does not, and a stable merge costs a constant factor that an unstable quicksort does not; both are the accepted price of those properties. No benchmark of that constant factor is committed with the port, so none is quoted here |
| 15 | Primitive callbacks | Java uses its own `collect.function.*` interfaces (`DoubleTernaryOperator`, `IntIntDoubleConsumer`, …) | Those interfaces are not ported (section (b)). Where a primitive callback is needed to keep a hot path free of boxing, the type is a single-abstract-method trait declared beside its user: `DoubleArray.DoubleTernaryOperator` (taken by `combineReduce` instead of a three-argument function) and `DoubleMatrix`'s `ElementAction`, `ElementFunction`, `RowArrayFunction`, `RowArrayObjectFunction`. Call sites stay ordinary Scala lambdas | `Function3` and friends are specialised over nothing, so the standard function types cannot satisfy the no-boxing requirement for these methods |
| 16 | Text loading | `ResourceLocator` decodes leniently, substituting a replacement character for malformed input, and reads a resource of any size | `io.Resources` decodes UTF-8 **strictly** — malformed or unmappable input fails the effect with an `IOException` naming the source — and refuses a source larger than its `MaxBytes` ceiling (64 MiB), which is private to `Resources` and met only as the failure that names it. A read that stops making progress is bounded too — row 50. `readFileText` is an unconfined filesystem reader, exactly as the Java original was: its only callers are in test scope — `ResourcesSpec` — and **callers must not pass it an untrusted path** | A substituted character inside a captured baseline is a silently altered expectation: the measurement still runs, against a value nobody captured |
| 17 | Equality and ordering | Joda-Beans equality on `double` fields via `doubleToLongBits`; `compareTo` ignores some fields | The same IEEE bit semantics — `NaN` is reflexive, `-0.0 ≠ 0.0` — implemented with `java.lang.Double.compare`/`hashCode` and `java.util.Arrays.equals`/`hashCode`, with one equality-bearing cats instance per type. `Order` additionally tie-breaks on the fields Java's `compareTo` ignored, so `compare == 0` holds exactly when `eqv` does: `FixedScaleDecimal` by decimal then scale, `Tenor`/`MarketTenor` by length then name, `SchedulePeriod` by unadjusted then adjusted dates, `CurrencyAmount`/`Money`/`BigMoney` by currency then amount. `MultiCurrencyAmount`'s `Monoid` laws are stated over finite amounts with a 1e-9 relative `Eq`, because IEEE addition is only approximately associative. These four tie-breaks are the **only** departures from Java comparison; every other `compareTo` is reproduced as it stands | A cats `Order` that disagrees with equality breaks every sorted collection built from it |
| 18 | `Collections.groupByPreservingOrder` | Guava's `MapStream` grouping | Returns `scala.collection.immutable.VectorMap` — an insertion-ordered immutable map, so iteration order and therefore serialized bytes stay deterministic, with linear assembly | The insertion-ordered `ListMap` that would otherwise express this is quadratic to build |
| 19 | Parity harness tolerance | — | The harness compares finite expectations within 1e-9 absolute **and** relative, and non-finite expectations by exact IEEE identity: two NaNs match, each infinity matches itself, anything else involving a non-finite value differs | `\|NaN − NaN\| <= 1e-9` is false, so a tolerance comparison would fail every non-finite row in the fixtures |
| 20 | Composite calendar id inside a linked id | `HolidayCalendarId.of("EUTA+GBLO~USNY").resolve(ReferenceData.standard())` **throws** `ReferenceDataNotFoundException: Reference data not found for 'EUTA+GBLO' of type 'HolidayCalendarId' when finding 'EUTA+GBLO~USNY'` (measured), because each `~` part is looked up raw in the store and a composite part is not stored | Resolves: `Right` of a linked calendar named `EUTA+GBLO~USNY` (measured), because a composite part is resolved by asking it, which tries its own whole name and then its parts. Strictly more permissive — it can only turn a Java failure into a success, never the reverse. See (c)-20 below | AAP §0.6.5 describes resolution as "resolve each component, then `combinedWith`/`linkedWith`", and a part that is itself composite is a component like any other. Java's own `findValue` answers `true` for the same composite id at top level, so its failure inside a linked id is an inconsistency rather than a rule |
| 21 | `DayCount.ofBus252` | `ofBus252(id)` resolves the calendar against `ReferenceData.standard()` internally, and `"Bus/252 XXXX"` is parsed by loading the calendar from the standard set — an ambient lookup inside a pure calculation | `Bus252` carries the **resolved** calendar, so `yearFraction`/`days` are pure functions of their arguments. It is built by `ofBus252(calendar)` (total) or `ofBus252(id, refData)` (`Either`). `parse(name)` resolves `Bus/252 X` against the built-in constant set, `parse(name, refData)` against supplied data | Reference data is threaded explicitly; no ambient global state |
| 22 | `RollConvention.IMMCAD`/`IMMAUD`/`TBILL` | Capture `GBLO` + `CATO`/`CAMO`, `AUSY` and `USNY` from `ReferenceData.standard()` at class-initialisation time, with a `SAT_SUN` fallback | Reference the built-in `StandardHolidayCalendars` constants directly — the same fixed calendars, as data rather than as a lookup, so this is retained fixed-calendar behaviour rather than a lost one. `adjust(date)` keeps Java's signature. `SFE` (second Friday) and `IMMNZD` take part in no calendar on either side | Same reason as row 21; these calendars were always constants |
| 23 | `DayCount.ScheduleInfo` | `getStartDate`, `getEndDate`, `getPeriodEndDate`, `getFrequency` throw `UnsupportedOperationException` by default; `Schedule.getPeriodEndDate` throws for a date in no period | The accessors are `Option`-valued and `ScheduleInfo.simple` answers `None` everywhere; `Schedule.periodEndDate(date)` is `None` outside every period. A day count that needs information it was not given still fails fast, per row 2 | Absence is a value, not an exception |
| 24 | `ImmutableHolidayCalendar` | `of` is total for any holiday dates; the Joda-Beans form carries the internal lookup array | `of` is total in signature but has a documented fail-fast precondition: every holiday must fall in years 0–9999, since the year range is what the storage is allocated from. The structural JSON form carries `id`, `weekendDays`, `startYear`, `holidays` and the weekend dates declared working; a decode routes through a private range-preserving factory when `startYear` is declared. A round trip preserves the first year and every reported date, but not a trailing tail of years holding no reported date, so `endYearExclusive` can come back smaller — every date in that tail answers identically either way | An unbounded allocation driven by an argument is a denial-of-service hazard; and a holiday that fell at a weekend is indistinguishable from the weekend once stored, so the end of such a range cannot be recovered from the dates |
| 25 | `ReferenceData` store | `ImmutableReferenceData.getValues()` publishes the erased `Map<ReferenceDataId<?>, Object>`; each entry's type is checked reflectively at insertion | No `values` accessor exists, and there is no raw-map factory: a store is read through `findValue`/`getValue`/`containsValue`, and built from `Entry[T]`, which binds value type to id type at compile time. One localized cast remains at lookup, closed for every Scala route; it could only be broken by raw-typed Java, which the port has no façade for | Removing the reflective per-entry check (Rule 6, D-5) is only sound if the erased store is not published |
| 26 | `Frequency` | `ofYears(1)` is `P1Y` and is **not** equal to `P12M`; `ofMonths(30)` renders `P30M`; `normalized()` maps a 12-month frequency to `P1Y` (all measured) | Normalised at construction: a year is held as twelve months, so `ofYears(1)` and `of(P1Y)` both render `P12M` (measured) and equal `P12M`; `ofMonths(30)` renders `P2Y6M` (measured); `normalized` is the identity | The AAP requires a normalised `Period`, the constants are named `P1D`…`P12M`, and every captured Java parity baseline spells the annual frequency `P12M`. Once every value is canonical there is nothing for `normalized` to do, and a length has exactly one name |
| 27 | `ValueAdjustmentType` members | The Java enum constants are spelled `REPLACE`, `DELTA_AMOUNT`, `DELTA_MULTIPLIER`, `MULTIPLIER`, and format to the canonical names `Replace`, `DeltaAmount`, `DeltaMultiplier`, `Multiplier` | Scala code refers to the members as `Replace`, `DeltaAmount`, `DeltaMultiplier`, `Multiplier`. The canonical names, JSON form, `Show` output and parse surface are identical to Java's, and the Java constant identifiers stay resolvable as lookup keys (`valueOf("DELTA_AMOUNT")` answers `DeltaAmount`) | AAP §0.4.1 fixes the required identifiers for this file; nothing reachable by name is lost. Scala source written against the Java constant spellings must be adjusted — no such source exists |
| 28 | `SequenceDate` rendering | `toString` contains the platform's absent-reference token for whichever of the two mutually exclusive fields is unset | Byte-identical at runtime; the token is obtained from the platform rather than written into the source, and the scaladoc examples stand it in as `[absent]` | The literal token in main sources would breach the build's no-`null` gate while changing nothing observable |
| 29 | JSON detail | Joda-Beans wire forms | The port's own shapes, described in section (e). Four deliberate narrowings: the `LocalTime` encoder is the port's own so that a time renders `11:00` rather than `11:00:00`; the tagged-double decoder refuses a JSON *number* that is not finite, since the tags are the only spelling the encoder produces for those values; the `Rounding` decoder rejects an unknown field rather than ignoring it; and every decoded collection carries a published ceiling, measured from the payload before an element is read — `Codecs.MaximumArrayElements` (1 048 576) for an array of doubles, `MaximumMatrixRows`/`MaximumMatrixColumns` (4096 each) and `MaximumMatrixElements` (1 048 576, checked as a 64-bit product so a stated shape cannot wrap) for a matrix, and `MaximumCollectionElements` (100 000) for a bounded collection field of a value: the holiday and working-day lists and weekend days of a calendar, a schedule's periods, a value schedule's steps, an `AdjustableDates` run, a `MultiCurrencyAmount`'s amounts and an `FxMatrix`'s currencies | The first three accept strictly less than a lenient reading would, and only payloads the port could never have written. The ceilings bound what a document can make the reader allocate: without them the size of every decoded run is the document's choice, and a few kilobytes of repeated text name gigabytes of values. The collection figure is the one the library already enforces on its own expansions (`PeriodicSchedule.MaximumPeriodCount`, `ValueStepSequence.MaximumStepCount`), so no value the port can build is refused by it, and `decode(encode(x))` still holds for every `x` |
| 30 | Serialization compatibility | Joda-Beans JSON and binary, plus `java.io.Serializable`/`Externalizable` | Neither is supported or tested, for any type. `ImmutableHolidayCalendar-Old.json` is not readable, `assertJodaSerialization` has no counterpart, and the `ImmutableHolidayCalendar` JSON shape is the port's own | Reflective serialization is what design decision D-5 and Rule 6 remove |
| 31 | Holiday calendar cache | `GlobalHolidayCalendars.bin`, a generated cache read at class-initialisation time, and `main` method that writes it | Not carried. The 25 generators are pure Scala functions, memoised as `lazy val`s, so a calendar is generated once, on first use | A binary cache in the classpath is the runtime-loading mechanism this port removes |
| 32 | Test-source and manifest details | — | Every test source is one the AAP sanctions: each is named in section 0.3.1 or is the `<Type>Spec` of a retained Java test class, `ScheduleFailureSpec` being section 0.2.2's rename of `ScheduleExceptionTest`. `java-test-mapping.csv` carries **no** method-level `dropped` row: all 18 of its `dropped` rows belong to the five test classes section 0.2.2 excludes whole — `HolidayCalendarIniLookupTest` (8 rows) and the four collect exception tests, `FailureExceptionTest` (1), `FailureItemExceptionTest` (3), `IllegalArgFailureExceptionTest` (3) and `ParseFailureExceptionTest` (3) — so every method of every retained Java class names a real Scala test case. The two manifest details that remain are shape rather than scope: `scala_test_name` is quoted per RFC 4180 on the 79 rows whose test name contains a comma, and two overloaded Java methods are qualified with an erased parameter list | Each was required to make a contract testable or a manifest unambiguous. The gate script must parse the manifest with a comma-tolerant CSV reader, and it permits `dropped` only for those five wholly excluded classes — a method-level exclusion inside a retained class is exactly what that row exists to catch |
| 33 | Failure rendering of caller-supplied text | A message quotes the text handed to a parse or a check as it stands, and the failure is written out the same way — unbounded and unescaped — so a log line is as large as the input and a line break in the input puts one in the line (measured against the `2.12.74-SNAPSHOT` jars, message length for a ten-thousand-character input: `BusinessDayConvention.of` 10,038, `Currency.of` 10,073, `CurrencyPair.parse` 10,023, `CurrencyAmount.parse` 10,040, `FxRate.parse` 10,014, `StandardId.parse` 10,027, `StandardId.of` with a rejected scheme 10,072, `ArgChecker.matches` 10,058; and `BusinessDayConvention.of("EUR\nUSD")` reports `BusinessDayConvention name not found: EUR`, a line feed, then `USD`). Java's `Decimal` is the one exception: its scanner threw a `NumberFormatException` naming the offending character rather than the text (measured), so the text this port quotes there is the port's own wording | The message is the Java one, character for character: every failure quotes the whole of what it rejected, and `Failure.message`/`Failure.attributes` hand that text back unchanged, so a caller correcting its input is given exactly what was refused. What differs is the **writing out**. `Failure.show` — and the text form of every failure, which delegates to it — renders each part (the message, and the key and value of every attribute) through one bounded renderer — `Failure.renderDiagnostic`, which is `private[strata]` rather than published API, its callers being the three places these two modules write a diagnostic: `Failure.show`, and therefore the text form of every failure; the bridge in `Codecs` that turns accumulated failures into a decoding failure; and the source names the resource reader quotes — which writes at most 512 characters per part plus a three-character marker, with a line feed written `\n`, a carriage return `\r`, a tab `\t` and every other ISO control character together with U+2028, U+2029 and a lone surrogate written as a six-character `\uXXXX` escape. A part within the bound and free of those characters renders byte-identically, so every ordinary failure reads exactly as the Java message did. Measured on this build: `Currency.of` with a ten-thousand-character code carries a 10,025-character message and renders to a 542-character single line; `Failure.Parsing("3M\nINJECTED")` renders on one line. The JSON form carries the text whole, escaped by the JSON grammar. The renderer reaches nothing else, deliberately: the text form of a `Named` value is its **name**, whole and unaltered — `HolidayCalendarId.toString`, its `Show` and the rendering of a typed string each answer with the name the value was built from, however that name is shaped — because AAP §0.1.1 makes the name the contract and a rendering that shortened or escaped it would make a name depend on a log-safety policy. A definition or an adjustment that quotes such an identifier therefore carries it as it stands as well, and the bounding happens where the failure reporting the rejection is written out; `HolidayCalendarIdSpec`, `TypedStringSpec`, `ScheduleFailureSpec` and `StubConventionSpec` each assert both halves on the same hostile name. One asymmetry remains deliberately: `ArgCheck` precondition failures are **thrown** rather than returned, are outside this contract, and echo their argument as it stands, as Java's `ArgChecker` did (measured: `ArgCheck.matches` with a ten-thousand-character argument throws a 10,058-character message, the same length Java's `ArgChecker.matches` produced) | A failure is a value that code acts on and a line that a person reads, and the two need different things: parity and full fidelity in the value, a bounded single line at the sink. Neutralising on output is what stops text that reached the library from outside it forging a line of a log (CWE-117) or making that line as large as itself, without restating what any message says |
| 34 | Bounded date generation | Date generation is unbounded: `PeriodicSchedule` and `ValueStepSequence` each materialise as many dates as the definition implies | Both refuse a definition asking for more than 100,000 items — `MaximumPeriodCount` periods, `MaximumStepCount` steps — so a definition Java would have generated, slowly, can be refused here. See (c)-34 below | An unbounded walk chosen by a caller's own frequency is a denial of service; the ceiling is a constant far above any real schedule |
| 35 | Date arithmetic at the calendar's edges | A roll or step leaving the range `java.time` represents raises `DateTimeException` or `ArithmeticException` out of schedule generation and step-sequence expansion | Those two exceptions, and no others, are caught around the stepping and reported as an invalid definition, so an `Either`-returning member stays one at the extremes of the calendar. See (c)-35 below | Row 1 applied to the one data-dependent failure that channel could not otherwise describe |
| 36 | Refusals moved into construction | `Schedule` validates only that its periods are non-empty; `ValueSchedule` has no validator; the three-argument `DaysAdjustment.ofBusinessDays` takes a zero-day business-day addition with a named calendar as given | `Schedule.of` requires the periods in chronological order and non-overlapping — no period's end after the next one's start, checked in the unadjusted and the adjusted pair alike, a gap between two periods being allowed; `ValueSchedule.of` refuses two steps at one position with different adjustments; `DaysAdjustment.of` refuses a zero-day business-day addition naming a calendar other than the no-holidays one. An input Java accepted is refused. See (c)-36 below | A value that cannot be built wrong needs no consumer to re-check it |
| 37 | Deprecation | `@Deprecated` marks seven `IborIndices` constants, two `OvernightIndices`, two `FloatingRateNames`, and the two superseded accessors of each of `Money` and `BigMoney` | No `@deprecated` annotation exists anywhere in the module. The seven `IborIndices` constants are the only members carrying a Scaladoc `@deprecated` tag, which names the date publication stopped; the two retired `OvernightIndices` constants, the two retired floating rate names and the superseded `getAmount` of each of `Money` and `BigMoney` are documented in ordinary prose that names what to read in their place and says no annotation is attached. Publication state is carried by the index's `active` flag. See (c)-37 below | A warning-as-error build that forbids suppression cannot name an annotated constant in a test or the demo; the state a caller needs is data, not a diagnostic |
| 38 | Hash codes stable across runs | Generated bean `hashCode`s seed their mixing with `getClass().hashCode()`, whose value depends on the run; 33 files of the Java module do this | No hash code here reads `getClass`, so every one is a function of the value alone, identical in every run and different from the Java value. The mechanism is chosen per type: eight types mix into a `HashSeed` constant that is the hash of their own type name, a value identified by a single field hashes by that field, and the rest fold their fields directly or take the derived product hash. Equality is untouched. See (c)-38 below | A hash that changes between runs cannot be written down or compared across processes |
| 39 | Product rendering | A bean's text form lists every property, present or absent, between braces | A product renders only what it holds, in a form of its own rather than a generated one: `PeriodicSchedule` its required four and whichever optionals are present, parenthesized — which is also the text a rejected definition carries as its `definition` attribute, so rendered definitions differ from the Java ones; `ValueStep` braced, naming only the position it holds; `SchedulePeriod` as a date phrase, adding the unadjusted pair only when it differs. `ValueAdjustment` renders its calculation in square brackets exactly as Java does, and the named types and `CalculationTargetList` reproduce the Java text exactly. See (c)-39 below | An absent optional field has nothing to print |
| 40 | Case-tolerant parsing of two families | `StubConvention.of` and `FloatingRateType.of` resolve through an exact map; neither family had a lenient lookup | `parse` applies the same upper-case lenient step every named family of this port uses, so any case of a member's name resolves; the alternates are unchanged. See (c)-40 below | One lookup algorithm for every named family, rather than two |
| 41 | `Country` interning | Each value is interned in a growing map, so identity comparison happens to work and `getAvailableCountries` grows by one whenever an unlisted code is first requested | A factory builds a fresh value, compared by `equals`/`Eq` and never by identity, and `availableCountries` is a fixed set of 252 — the 251 alpha-2 codes of the built-in table plus `EU`. See (c)-41 below | A published set that changes with what a program has already asked for is not a property of the library |
| 42 | Construction closure on the JVM | `sealed`, a `private`/`private[pkg]` constructor and the absent `apply`/`copy` are checked by scalac and by nothing else. In the class file this language version emits no `PermittedSubclasses` attribute and a `private` constructor becomes **public**, so every family base class and every validated type is an ordinary extensible public abstract class with a reachable constructor — a property the Java module relied on, `ExtendedEnum` having been designed to be extended at run time | Every one of the 33 `sealed abstract case class _ private` types and the 15 `sealed abstract class _ private[pkg]` families runs a construction guard as the first statement of its own body: `JvmClosure.requireSoleImplementation(this, classOf[X.Impl])` for a validated or normalising type, `JvmClosure.requireDeclaredMember(this, classOf[Family])` for a named family. Each type's implementations are `private final class`es declared in its own companion (or, for `DayCount.Bus252`, in the companion of the family that guards it), so the class file keeps them private too. A subtype compiled by another language raises `IllegalArgumentException` while running `super(...)` and never completes. Measured by the closure row of `scripts/verify-gates.sh`: 48 closed types call their guard in their own constructor bytecode, 90 hidden implementation classes are unnameable from Java — `javac` refuses every one with "has private access" — and 48 generated external Java subclasses compile, as the JVM permits, while none constructs (47 refused by the guard, one refused earlier because its base class derives a field from the argument). **Identity alone is not closure, and the second half of the mechanism is why.** A `private final class Impl` is emitted as an `ACC_PUBLIC` class with an `ACC_PUBLIC` constructor: only the `InnerClasses` attribute records the `private`, which `javac` honours — hence the 90 refused probes — and a class file emitted without a compiler does not, so the implementation's own constructor is reachable and a value built through it has exactly the runtime class the identity guard admits. Every closed type whose implementation carries state therefore also states, with `JvmClosure.requireInvariant`, the invariant its factory establishes, over the fields the instance holds: `Currency` that its code, minor units and triangulation currency are the row the reference data publishes for that code; `CurrencyAmount` that its amount is a number and its zero positive; `Money` that its amount is already rounded to its currency's minor units; `SchedulePeriod` that its dates run forwards; `Tenor` that its name is the one its period implies; and so on for 41 types, the invariant living on the class that declares the fields (`DayCount.Bus252`, `RollConvention.Dom`) where that is not the head of the family. No type carrying state is exempt: `CurrencyAmountArray` was the one exemption while its factory admitted every array of numbers, and it states the element invariant its factory now establishes - that every element of its values is a number - which the empty run satisfies vacuously. **Closure of the hierarchy roots**: `Index`, `FloatingRateIndex` and `RateIndex` are abstract **classes** rather than traits, because a trait compiles to a plain JVM interface that any class file may implement without running a constructor; `Index` runs `JvmClosure.requirePermittedSubtype`, which the levels beneath inherit; `IndexObservation` is deliberately not closed (row 46). Measured by the closure row of `scripts/verify-gates.sh`, in eight parts of which five are executed attacks: 51 closed declarations (48 closed types and 3 hierarchy levels) call their guard in their own constructor bytecode and 41 of 41 stateful ones state an invariant; 90 hidden implementations are unnameable from Java and 90 are reachable in bytecode; 48 external Java subclasses and 3 foreign subtypes of a hierarchy level compile, as the JVM permits, and none constructs; and 10 forged states pushed through the binary `Impl` constructors — reached by compiling against a stub tree that declares them under their binary names, so the attacker's bytecode is a plain `new`/`invokespecial` on the real constructor — are all refused by the invariant, named in the refusal. Subsection (c)-42 states what this does not cover | Rule 4's closed families and AAP §0.3.3's construction policy are what removes dynamic currencies, indices and conventions; a closure holding only for Scala callers does not remove them, and a validated value forged through a public constructor carries exactly the input its factory rejects (CWE-20, CWE-668) |
| 43 | Java serialization | Joda-Beans wire forms plus `java.io.Serializable`/`Externalizable`, and — in Scala — a `case object` that deserializes through the compiler's module proxy while a case class is populated field by field | Refused in **both** directions by every product of both modules. `collect.NoJavaSerialization` supplies the two inheritable hooks the JDK consults, `writeReplace` and `readResolve`, so `ObjectOutputStream.writeObject` raises before a byte is written and an object reconstructed from a forged stream raises before it reaches the caller that asked for it. Measured: all 193 compiled products of both modules refuse; the classes still taking part are the compiler's own encoding — 105 singleton companion modules and 155 derivation and lambda classes — none of which carries data of the library. **Both hooks are `final`**, which is the difference between a refusal and a convention: a subclass overriding them to return itself would be written and read normally, its fields populated by the stream and no constructor run, so the compiler emits them `ACC_FINAL` on every class that mixes the trait in and the JVM rejects such a class when it is **loaded**. The gate mounts that attack rather than asserting it — a stub declares the type with overridable hooks, a subclass overriding both is compiled against it, a real object stream carrying that subclass is written, and reading it against the real classes fails inside `ObjectInputStream.readObject` with `IncompatibleClassChangeError: class attack.ForgedDecimal overrides final method ...readResolve` — and `ApiSurfaceSpec` asserts over the emitted methods of all 193 products that neither hook is overridable. Two visible consequences: the refusal is an `IllegalArgumentException` rather than `NotSerializableException`, because Rule 5's gate confines `throw new` to `ArgCheck.scala`, and a `case object` of these modules no longer round-trips through `ObjectInputStream` where Java's would | Row 30 takes Joda and Java serialization out of the contract, but the compiler's `Serializable` supertype leaves `ObjectInputStream` as a second construction path that fills fields no factory validated and no decoder checked (CWE-502) |
| 44 | Shape of the fail-fast checks | `ArgChecker`'s value checks return the checked argument, so a check can be written inline in a field assignment; `isTrue` has five forms — one with no message, one with a plain message, and three taking a message template with `Object...`, `long` or `double` arguments — and `isFalse` two, both message-bearing; an iterable and a collection have separate emptiness checks (`ArgChecker.java:557` and `:587`, in that order) | Every `ArgCheck` member returns `Unit` and is called in statement position; one `isTrue(Boolean)`, one `isTrue(Boolean, => String)` and one `isFalse(Boolean, => String)` take their message by name and build it only on failure; one `notEmpty(Iterable[T], String)` covers both. The exception type is unchanged, and so is the message of every check that survives with its Java counterpart - with one exception created by that last collapse: an empty *collection* is reported `Argument iterable '<name>' must not be empty` where Java's collection overload said `Argument collection '<name>' must not be empty`. See (c)-44 below | A discarded value is a compile error in this build, so returning the argument would force every call into a throwaway binding; interpolation at the call site is compiler-checked where a template is not; and the throw stays direct because `require` would prefix every message that reaches a log or a test expectation |
| 45 | What a failure carries | `FailureItem` holds a reason, a message, attributes, a **stack trace** built at construction - synthesized from the current thread's frames for a message-built failure, taken from the supplied throwable for a `Throwable`-built one, and inherited from the underlying item when a `FailureItemProvider` is wrapped - and an optional **cause type**, the class of that throwable | `Failure` holds the reason, the message and the attributes, and nothing else. Neither a trace nor a cause type is captured, rendered by `Failure.show` or carried in the JSON form of section (e), and constructing a failure costs no stack walk. See (c)-45 below | A failure here describes what went wrong with the data - the part a caller acts on and a reader reads. Where the frames matter they belong to the `IO` edge that escalates the failure into an error, which is raised at that point |
| 46 | `IndexObservation` — the AAP's wording against the AAP's file layout | — | `IndexObservation` is an open `trait` declaring one member, `index: Index`, and the four observations this module publishes — `IborIndexObservation`, `OvernightIndexObservation`, `PriceIndexObservation`, `FxIndexObservation` — extend it from a file of their own. AAP §0.4.1's key-changes cell for `index/IndexObservation.scala` writes `sealed trait IndexObservation`, while §0.3.1, the authoritative file layout, gives those four observations files of their own; Scala 2 admits a direct subtype of a sealed type only in the file that declares the type, so the two cannot both hold. The port follows §0.3.1, and Rule 4 agrees: the families it names as closed are `Index` with its leaf families, the calendars, `Rounding` and the failure model, and `IndexObservation` is not among them. Java's `IndexObservation` is a plain interface, so **nothing observable in Java's behaviour changes** and an application may still implement the trait and be carried by every signature written over it. One consequence follows for a caller: the set of observations is not closed, so a `match` over the trait is not checked for exhaustiveness and narrowing code writes a default branch, exactly as code over the Java interface tested with `instanceof` did. `ApiSurfaceSpec` carries the trait's row among the open contracts and compiles an implementation declared outside `IndexObservation.scala` | This row reconciles §0.4.1's wording with §0.3.1's layout; it is **not** an accepted departure from the frozen per-file plan, which documentation could not authorise in any case. The delivered tree matches §0.3.1 file for file (`index`: 18 main sources), and the four observation files are what sealing the trait would have cost. Openness is also the reading that keeps the extension point the ported interface offers |
| 47 | Bounded calendar walks | `shift` loops `next`/`previous` over the count it is handed and `ImmutableHolidayCalendar` walks its stored months, with no ceiling on that count (`HolidayCalendar.java:97-109`, `ImmutableHolidayCalendar.java:419-433`); the default `next`/`previous` step one day at a time until a business day is found, with no ceiling on the closed days crossed (`HolidayCalendar.java:120-123,150-153`); and `DaysAdjustment` holds whatever day count it is given, so an adjustment read from a document decides how long applying it takes. No such limit appears in any of those sources. A calendar with no business day at all is also constructible: `ImmutableHolidayCalendar.of` applies no check to the weekend days it is handed, so all seven of them may be closed | Two ceilings, both **public** members of `HolidayCalendar`. `MaxBusinessDayShift` (100 000 business days, more than three hundred years of them) bounds the count, checked before the walk in `shift`/`adjustBy`, in `ImmutableHolidayCalendar`'s own override, and again where a `DaysAdjustment` is *built* rather than applied — so an adjustment that exists can always be applied. `MaxConsecutiveHolidays` (3 653 days, ten years of closure) bounds the search for a business day, refusing after that many closed days and naming the calendar and the date. Beside them, a weekend closing all seven days is refused at construction and by the decoder (`must leave at least one day of the week open … so it has no business day`), so the walk bound is reached only by a composite whose parts between them close every day. See (c)-47 below | The count of a shift and the closure of a calendar are both chosen by data — an adjustment arrives in a document, a composite is assembled from calendars a caller supplies — so without a ceiling a document decides how long a calculation runs and whether it ends at all (CWE-400/CWE-835). Both figures are constants far above anything finance states, and neither is a parameter, so no caller can raise one |
| 48 | Bounded calendar composition | `combinedWith`/`linkedWith` compose without limit, and every recursive operation of the family — deciding whether a date is a holiday, composing the identifier, writing the calendar out, reading one back — walks the resulting tree; `HolidayCalendarId.of` decomposes a name of any length into any number of parts, sorts them and rejoins them (`HolidayCalendarId.java:87-120`). No ceiling appears in either Java source | `HolidayCalendar.MaxCompositeDepth` (128, another **public** member) is applied wherever a composite comes into being: `combinedWith`, `linkedWith`, the `Combined` and `Linked` constructors, the reading of a calendar from a document, and `HolidayCalendarId.resolve`, which reports it in its own failure channel so that resolving an identifier still answers rather than raising. `HolidayCalendarId.of` stays **total**, and what is bounded there is the work: a name beyond `MaxCompositeNameLength` (65 536 characters) or `MaxCompositeParts` (4 096 parts) is kept whole as one opaque identifier rather than decomposed. See (c)-48 below | A tree tall enough exhausts the stack, which ends the calling thread rather than the calculation, and decomposing a name costs its length plus its parts times their logarithm — both unbounded in text a caller supplies (CWE-400/CWE-674/CWE-770). Nothing about combining calendars requires height: a payment settling in every centre this library knows reads some thirty calendars |
| 49 | Bounded parse text | No text-bearing parse has a length ceiling. `StandardId.of` states the permitted lengths of its two parts as `1, Integer.MAX_VALUE` (`StandardId.java:165-166`); `CurrencyAmount.parse` tests only a minimum (`CurrencyAmount.java:120`); `FxRate.parse`, `Tenor.parse`, `MarketTenor.parse` and `Frequency.parse` test none. The scanning, case folding and copying each performs is therefore proportional to text whose length the sender chose | Each of those states a ceiling and tests it **first**, before the text is folded, copied or scanned: 65 536 characters per `StandardId` part and 131 073 for its whole text, 1 024 for the amount text of `CurrencyAmount` and the rate text of `FxRate`, and 256 for `Tenor`, `MarketTenor` and `Frequency`. Text past the bound is reported with the wording of a value that could not be read rather than with a wording of its own. See (c)-49 below, which also lists the three figures in this area that are **Java's own**, restated rather than added | Refusing a name should cost what the name costs: without a ceiling, the work of a refusal is proportional to text arriving from outside the library and chosen by its sender (CWE-400/CWE-770). Each figure is derived from what the grammar can express rather than from what a machine can hold, which is what keeps it a bound on work rather than on meaning |
| 50 | Bounded reading time | `ResourceLocator` reads until the source ends. A source that never yields — a named pipe, a device, a stream whose writer has stopped without closing — holds a thread and a file handle for as long as it chooses; no Java source carries a time bound | `io.Resources` abandons a read that has not completed within its `ReadTimeLimit` of two minutes, applied with `timeoutAndForget` so that the caller is not made to wait for a read that will not finish, and fails the effect with an `IOException` naming the source and the bound. Like `MaxBytes` (row 16) it is private to `Resources` and met only as that failure, and the two are chosen the same way: the largest text in this repository is the day-count parity baseline at roughly 12.7 MiB, which reads in well under a second, so a read that is progressing has two orders of magnitude of headroom. A legitimate read slower than this would be refused; none in this tree comes near it | Row 16 bounds how much a source may deliver; this bounds how long it may take not to deliver it, which is the other way a source can hold a thread of the blocking pool and a handle of the process without limit (CWE-400) |
| 51 | Bounded diagnostics | A diagnostic quotes as much as it has. `FxMatrixBuilder` lists every rate it could not place, rendering the whole collection into the message (`FxMatrixBuilder.java:69`), and a message naming a class or restating a reason names or restates the whole of it | Four elisions, each marked where it cut. `FxMatrix` lists at most `MaxListedRates` (8) of the rates a fold could never place and then says how many more there were, walking an iterator rather than taking a prefix, so describing a large collection of unplaceable rates builds the bounded listing and nothing else. A decoding failure carries the messages of at most `Codecs.MaxReportedFailures` (10) accumulated causes and then `and N more`. A factory that refused a payload by raising has its account rendered to at most `MaxRaisedAccount` (256) characters plus a three-character marker. A construction-closure refusal (row 42) names at most `MaxDescribedNameLength` (200) characters of the class it refused, with every control character in it replaced. The **failure values** are untouched: this is the writing out, as row 33 is | How much there is to report is the payload's choice, so a report that grows with it describes the sender rather than the fault (CWE-400/CWE-770). Ten causes is above what any factory of this port accumulates — every failure the library itself produces still names every cause — so the bound is reached only by a payload that drives a factory to accumulate a cause per element, and a reader is always told that a report was cut |
| 52 | A run of amounts holds no NaN | `CurrencyAmountArray` and `MultiCurrencyAmountArray` accept any `double` in their values: neither Java source examines an element, so a run could hold a value that is not a number while `CurrencyAmount` itself rejected one (`CurrencyAmount.java:148`) | An element of a run is an amount, and an amount is a number, so the examination sits at the one construction point of each type. The members that are **total in signature** therefore **throw** `IllegalArgumentException` through `ArgCheck`, naming the offending index: `CurrencyAmountArray.of(currency, DoubleArray)`, which AAP §0.4.1 makes total, and the `multipliedBy` and `mapAmounts` of both types, which were total in the types being ported (row 2). Every route that has a failure channel reports it instead, and the invariant is restated on each implementation class with `JvmClosure.requireInvariant`, over the fields the instance holds, so a run forged through the binary constructor is refused too (row 42). The two infinities are values an amount holds and are admitted, and the empty run satisfies the invariant vacuously. See (c)-52 below | A run holding a value that is no amount is a value whose every later reader fails on it, at a distance from where it was built (CWE-20). Establishing the element invariant at the one construction point is what makes the type impossible to hold in that state, and a NaN produced from numbers is the numeric-domain edge AAP §0.3.3 classes as a fail-fast `ArgCheck` throw |

### (c)-3 — Null arguments are outside the contract

The Java module guarded `null` at nearly every public entry point with `ArgChecker.notNull`, which
this port does not carry (section (a), row 10): in Scala absence is `Option`, and a `null` reaching
one of these methods is a defect in the calling code rather than a data condition to describe. The
consequence is visible and it is not uniform.

Measured on this build, beside the Java value of the same expression against the `2.12.74-SNAPSHOT`
jars:

| Entry point | Java | Port |
|---|---|---|
| `StandardId.of(null, "v")` | `IllegalArgumentException: Argument 'scheme' must not be null` | `NullPointerException: Cannot invoke "String.length()"` |
| `StandardId.parse(null)` | `IllegalArgumentException: Argument 'str' must not be null` | `NullPointerException: Cannot invoke "String.indexOf(String)"` |
| `CurrencyPair.parse(null)` | `IllegalArgumentException: Argument 'pairStr' must not be null` | `NullPointerException: Cannot invoke "String.toUpperCase(java.util.Locale)"` |
| `CurrencyAmount.parse(null)` | `IllegalArgumentException: Unable to parse amount, invalid format: null` | `NullPointerException: Cannot invoke "String.length()"` |
| `FxRate.parse(null)` | `IllegalArgumentException: Argument 'rateStr' must not be null` | `NullPointerException: Cannot invoke "String.toUpperCase(java.util.Locale)"` |
| `Currency.of(null)` | `IllegalArgumentException: Argument 'currencyCode' must not be null` | `Left(Failure.Parsing("Currency name not found: null"))` |
| `Currency.parse(null)`, `Country.of(null)`, `Frequency.parse(null)` | `IllegalArgumentException: Argument 'currencyCode' / 'countryCode' / 'toParse' must not be null` | `NullPointerException` at the first dereference |
| `HolidayCalendarId.of(null)` | `NullPointerException` — Java dereferences the null cache key before any check | `NullPointerException: Cannot invoke "String.contains(java.lang.CharSequence)"` |

**No public entry point of either module accepts `null`, and none promises a particular failure for
it.** `Currency.of` answering `Left` is an artefact of a table lookup — a missing key is a missing
key — not a guarantee, and no other entry point matches it. Hold a possibly-absent string as
`Option[String]` and decide what absence means before calling: `Failure` describes *data* that is
wrong, and a missing reference is not data. Java rejected `null` too, so nothing here is a parity
regression; only the exception type differs.

### (c)-5 — Name registration when two names differ only in case

Java registered a family's names two different ways, depending on how the family was declared. A
family read from configuration registered the declared spelling unconditionally and the English
upper-case spelling only where it was free — `mutableMap.put(key, name)` then
`mutableMap.putIfAbsent(key.toUpperCase(Locale.ENGLISH), name)` in
`FloatingRateNameIniLookup.java:125-126`, which is the loader for `FloatingRateName`; the
alternate-name table was built the same way (`ExtendedEnum.java:255-256`). A family read from a
constants class registered both spellings conditionally —
`instances.putIfAbsent(instance.getName(), instance)` then
`putIfAbsent(instance.getName().toUpperCase(Locale.ENGLISH), instance)`,
`ExtendedEnum.java:232-233`.

`NamedEnum` implements the first rule for every family: a member claims its canonical name
unconditionally, and its upper-case spelling only where no member has claimed that key already. The
two rules agree for every family whose names are distinct once case is discounted, and they part
company where they are not: under the conditional rule the upper-case spelling of an earlier member
occupies the key that is a later member's own canonical name, and that later member stops being
resolvable by the name it reports. `FloatingRateName` holds two such pairs —
`DKK-DESTR-OIS Compound` beside `DKK-DESTR-OIS COMPOUND`, and the `SEK-SWESTR-OIS` pair — and Java
read that family from configuration, so the port answers those four names exactly as Java did.
Applying the one rule everywhere is what keeps every member of every family resolvable by its own
name, whatever the family is declared from.

### (c)-9 — Tolerance comparison at NaN and at an infinite tolerance

**What the AAP requires.** §0.3.3 directs `DoubleArrayMath` to reproduce Guava's semantics "including
`NaN` (never fuzzy-equal, not an integer) and infinities (fuzzy-equal only to themselves, not
integers)", and §0.4.2 restates it as "Guava semantics for `NaN`/infinities preserved". The rule is
therefore: a NaN is fuzzy-equal to nothing, including another NaN, and each infinity is fuzzy-equal
only to itself. Guava's own algorithm,
`copySign(a - b, 1.0) <= tolerance || a == b || (isNaN(a) && isNaN(b))`, does not do either — its
third clause makes two NaNs equal, and its first equalises any two non-NaN values once the tolerance
is infinite. Where the two disagree the AAP governs, so the divergence recorded here is from Guava.

**What the port implements.** One private comparison, which all five public entry points delegate to
— the scalar and array `fuzzyEquals`, `fuzzyEqualsZero`, `DoubleArray.equalWithTolerance` and
`equalZeroWithTolerance` — classifies both operands before it measures anything
(`DoubleArrayMath.scala:568`): a NaN on either side is equal to nothing, an infinity is equal only to
the same infinity whatever the tolerance, and only then are two finite values compared by
`abs(a - b) <= tolerance`. Taking the non-finite cases first is what makes that distance meaningful,
because by then both values are finite. Measured against the built classes of this tree:

- `DoubleArrayMath.fuzzyEquals(NaN, NaN, 0.1)` is `false`, and so is every other spelling of the
  tolerance — `0.0`, `MAX_VALUE` and `+Inf` alike. `fuzzyEquals(NaN, 1.0, 1e-9)` is `false` too, so a
  NaN is equal to nothing rather than to everything but itself.
- The array form and `DoubleArray.equalWithTolerance` inherit it: `[NaN]` against `[NaN]` at 1e-9 is
  `false`.
- Each infinity is fuzzy-equal to itself at every tolerance, the tightest included
  (`fuzzyEquals(+Inf, +Inf, 0.0)` is `true`), and to nothing else at any tolerance:
  `fuzzyEquals(+Inf, -Inf, MAX_VALUE)`, `fuzzyEquals(+Inf, -Inf, +Inf)`, `fuzzyEquals(+Inf,
  MAX_VALUE, +Inf)` and `fuzzyEquals(0.0, +Inf, +Inf)` are all `false`. An infinite tolerance
  therefore equalises no pair that a finite one does not, which is the second half of the rule.
- The zero-comparing variants answer from the same three cases:
  `fuzzyEqualsZero([NaN], 1e-9)` and `DoubleArray.of(NaN).equalZeroWithTolerance(1e-9)` are `false`,
  and `fuzzyEqualsZero([+Inf], +Inf)` and `DoubleArray.of(+Inf).equalZeroWithTolerance(+Inf)` are
  `false` as well — where `ArgCheck`'s private near-zero test, which keeps the Java reading for the
  two argument checks it serves, answers that an infinity *is* near zero at an infinite tolerance
  (row 10 covers both, and the scaladoc of each tells a reader to expect the difference). A NaN
  tolerance is rejected on both sides; only the message differs.
- The two zeroes compare equal at a zero tolerance (`fuzzyEqualsZero([0.0, -0.0], 0.0)` is `true`),
  since their distance is zero and neither is non-finite.

The other half of the same AAP sentence is met too: `isMathematicalInteger` treats neither a NaN nor
an infinity as an integer (measured: both `false`, `DoubleArrayMath.scala:514`).

**Status: compliant with the AAP, divergent from Guava.** A caller must not rely on two NaN-bearing
arrays comparing equal, nor on an infinite tolerance equalising distinct values, and a reader
comparing this comparison against Guava's should expect those two answers to differ. Numerical parity
depends on neither: no captured baseline carries a fuzzy expectation, and the parity harness compares
with its own delta (row 19).

### (c)-20 — A composite calendar id inside a linked id

`HolidayCalendarId.of("EUTA+GBLO~USNY")` names a calendar linked from two parts, the first of which is
itself composite. Both implementations try the **whole** name first, so reference data holding a
pre-combined calendar under this exact id answers with it; they differ in what happens next. Java
splits on the outer separator and performs a *raw* store lookup per part —
`refData.queryValueOrNull(splitId)`, a plain map read for `ImmutableReferenceData` — so the part
`EUTA+GBLO`, which standard reference data does not hold as a whole, is absent and resolution fails:

```
Java:  HolidayCalendarId.of("EUTA+GBLO~USNY").resolve(ReferenceData.standard())
       -> ReferenceDataNotFoundException: Reference data not found for 'EUTA+GBLO'
          of type 'HolidayCalendarId' when finding 'EUTA+GBLO~USNY'
Port:  HolidayCalendarId.of("EUTA+GBLO~USNY").resolve(ReferenceData.standard)
       -> Right(linked calendar, id and name "EUTA+GBLO~USNY")
```

The port resolves a composite part by asking it, which tries its own whole name and then its own
parts, and only then links the results. Two consequences, both measured:

- The divergence is **strictly more permissive**: every id Java resolves, the port resolves to the
  same calendar, and the port never fails where Java succeeds. `GBLO+USNY`, `EUTA+GBLO` and
  `GBLO~USNY` resolve identically on both sides. Java itself answers with a calendar for
  `ReferenceData.standard().findValue(HolidayCalendarId.of("EUTA+GBLO"))`, because that path goes
  through the id's own resolver; the same id fails only when it appears as a *part* and is looked up
  raw.
- A genuinely missing calendar still fails, all-or-nothing, and the two implementations report it at
  different depths: for `XXXX+GBLO~USNY` the port reports the innermost missing simple calendar
  (`Failure.MissingData`, attributes `id -> XXXX`, `compositeId -> GBLO+XXXX`) while Java names the
  composite part (`Reference data not found for 'GBLO+XXXX' … when finding 'GBLO+XXXX~USNY'`). The
  message text and attribute names are otherwise the Java ones.

### (c)-34 — Bounded date generation

Neither Java `PeriodicSchedule` nor Java `ValueStepSequence` caps the number of dates it generates:
each walk materialises as many boundaries as the dates and the frequency imply, so a daily
frequency over a span of centuries is attempted rather than refused (no ceiling appears in either
Java source).

This port refuses a definition asking for more than 100,000 items — `MaximumPeriodCount` periods in
`PeriodicSchedule`, `MaximumStepCount` steps in `ValueStepSequence`. `PeriodicSchedule` holds the
ceiling three times over: a span whose width makes it provably unreachable is refused in constant
time before either walk begins, so nothing a generation would have completed is refused; each walk
stops at the ceiling's worth of boundaries, so nothing beyond it is materialised; and the assembled
date list is checked against the ceiling exactly. `ValueStepSequence` holds it as each rolled date
is accepted. A definition Java would have generated, slowly, can therefore be refused here.

Two further constants belong to that constant-time preflight rather than being refusals of their
own, and are named here so that nothing in `PeriodicSchedule` looks like an undocumented bound:
`MaxRollAdjustmentDays` (31) is an upper bound on how far a roll convention's adjustment can move a
stepped date, and `MaxFallbackStepDays` (31) on the substituted one-month step a convention takes
where adding the frequency would not advance the date. They are used only to compute the widest
distance one generated step can cover, which is what makes the preflight **conservative**: it
answers that a span is too wide only when no walk over that span could stay within
`MaximumPeriodCount`, so it refuses nothing the exact checks would have accepted.

The ceiling sits far above any real schedule and is a constant rather than a parameter, so no
caller can raise it: an unbounded walk chosen by a caller's own frequency is a denial of service in
a library that values whatever it is handed.

### (c)-35 — Date arithmetic at the edges of the representable calendar

A roll or a step that leaves the range `java.time` can represent raises `DateTimeException` or
`ArithmeticException` out of Java's schedule generation and out of its step-sequence expansion;
neither Java source catches either type, so the exception escapes a method that otherwise reports
its failures.

Here those two exceptions, and no others, are caught around the stepping in `PeriodicSchedule` and
`ValueStepSequence` and reported as an invalid definition. A member that answers `Either` therefore
keeps answering `Either` at the extremes of the calendar. This is row 1 applied to the one case
that channel could not otherwise describe.

### (c)-36 — Three refusals moved into construction

Java builds three values this port refuses:

- `Schedule` validates only that its period list is non-empty (`validate = "notEmpty"`), so a list
  that runs backwards or overlaps is constructible. `Schedule.of` here requires that no period's
  end falls after the next period's start, checked independently for the unadjusted pair and the
  adjusted pair, so a list that runs backwards or in which two periods overlap is refused.
  Adjacency is not required: a gap between one period and the next is allowed, because a schedule
  may describe accrual that pauses.
- Java `ValueSchedule` carries no validator, so two steps naming one position are detected during
  resolution if at all. `ValueSchedule.of` here refuses two steps naming the same position with
  different adjustments.
- The three-argument `DaysAdjustment.ofBusinessDays` constructs a zero-day business-day addition
  that names a holiday calendar exactly as given. `DaysAdjustment.of` here refuses a zero-day
  business-day addition whose calendar is not the no-holidays one, because an addition of zero
  business days names no day. The two-argument factory behaves as Java's does, substituting the
  no-holidays calendar and carrying the named calendar in the business-day adjustment instead.

An input Java accepted is refused in each case. A value that cannot be built wrong needs no
consumer to re-check it, and a contradiction found at construction names the field that carried it.

### (c)-37 — Deprecation is documented, not annotated

`@Deprecated` marks seven `IborIndices` constants, two `OvernightIndices` constants, two
`FloatingRateNames` constants and the two superseded accessors of each of `Money` and `BigMoney`.

This port documents the same facts and attaches no annotation anywhere: `@deprecated` appears in
these sources only inside documentation, never on a declaration. Where it appears as a Scaladoc tag
it names the date the rate stopped being published, and the seven `IborIndices` constants are the
only members that carry one.

The rest are documented in ordinary prose instead, each naming the member or the flag to read in
its place and saying in as many words that it carries no deprecation annotation: the two retired
`OvernightIndices` constants, the two retired `FloatingRateName` constants, and the superseded
`getAmount` of each of `Money` and `BigMoney`, whose replacement `getValue` carries the scale
alongside the value rather than encoding it in a `BigDecimal`. Whether the rate behind an index is
still published is carried by the index's own `active` flag, which a caller reads and branches on.

The annotation is a compiler diagnostic, and this build compiles every warning as an error while
forbidding a suppression anywhere (section 0.10.1, Rule 9), so annotating a constant that the
tests, the demo and the reference-data manifest must name would make the build unbuildable. What a
caller needs is the publication state as data on the index, not a warning at its own call site.

### (c)-38 — Hash codes are stable across runs

Generated bean `hashCode`s seed their mixing with `getClass().hashCode()`, whose value depends on
the run. Thirty-three files of the Java module do this.

No hash code in this port reads `getClass`. Every one is a function of the value alone, so it is
identical in every run of every JVM and differs from the Java value for the same value. Equality is
untouched, as is the contract that equal values hash equally.

The mechanism is chosen per type rather than applied uniformly, and three shapes appear:

- eight types mix their fields into a `HashSeed` constant that is the hash of their own type name:
  `FxRate`, `CurrencyAmountArray`, `MultiCurrencyAmountArray`, `ValueAdjustment`, `ValueSchedule`,
  `ValueDerivatives`, `OvernightIndexObservation` and `FxIndexObservation`;
- a value identified by a single field hashes by that field, which is what the named families do:
  `name` for the four index families, the day counts and a reference data id, `code` for a
  currency, `id` for a holiday calendar, `externalName` for a floating rate name, and `store` for
  the immutable reference data;
- the rest fold their fields directly with no type-name seed: `CurrencyAmount` mixes currency and
  amount, `FxMatrix` its currencies and its rates, `MultiCurrencyAmount` folds its entries in
  currency order, and `IborIndexObservation` starts its mixing from the hash of the index.

Every type not in that list takes the hash the compiler derives for the product, which reads the
fields and nothing else. A hash that changes between runs cannot be written down, compared across
processes, or used to make a test deterministic, and none of these change.

### (c)-39 — A product renders the fields it holds

The text form of a bean lists every property, present or absent, between braces —
`PeriodicSchedule{startDate=2014-06-16, ..., stubConvention=null, ...}`.

A product here renders only what it holds, and the form is chosen per type rather than generated:

- `PeriodicSchedule` writes its four required fields and whichever of its seven optional fields are
  present, parenthesized: `PeriodicSchedule(startDate=2014-06-16, endDate=2014-09-16,
  frequency=P3M, businessDayAdjustment=...)`. That text is also what a rejected definition carries
  as the `definition` attribute of its failure, so a caller comparing rendered definitions sees a
  different string from the Java one.
- `ValueStep` keeps the braces and names only the position it holds —
  `ValueStep{periodIndex=2, value=...}` or `ValueStep{date=2014-06-30, value=...}` — where the
  bean printed both positions and rendered the absent one as its absence marker.
- `SchedulePeriod` renders a date phrase, `2014-06-16 to 2014-09-16`, adding
  `(unadjusted ... to ...)` only when the unadjusted pair differs from the adjusted one, where the
  bean printed all four dates under their property names. Its four fields are written out under
  their own names by the JSON encoding instead.
- `ValueAdjustment` renders the calculation it performs in square brackets,
  `ValueAdjustment[result = input + -2000.0]`, which is what the Java class does as well; this one
  is unchanged rather than divergent.

`Schedule`, `ValueSchedule`, `CalculationTargetList` and the named types are readable the same way,
from their own `toString`, and the last two reproduce the Java text exactly (section (e)). An absent
optional field has nothing to print, and a rendering that prints it prints the absence marker of
whatever held it.

### (c)-40 — Case-tolerant name parsing for two families

Java's `StubConvention.of` and `FloatingRateType.of` resolve through an exact map, so only a
published spelling or a registered alternate resolves; neither family had the lenient lookup the
configuration-backed families had.

`parse` on both families applies the same upper-case lenient step every named family of this port
uses, so any case of a member's name resolves. The alternates themselves are unchanged, and no new
spelling is accepted beyond a case variant of one that already resolved.

### (c)-41 — `Country` values are not interned, and the published set is fixed

Java interns each `Country` in a growing map keyed by code, so two requests for one code answer the
same instance and an identity comparison happens to work, and `getAvailableCountries` returns that
map's values — which grow by one each time a code outside the published table is first requested.

Here a factory builds a fresh value, compared by `equals` and by `Eq` rather than by identity, and
`availableCountries` is a fixed set of 252: the 251 alpha-2 codes of the built-in table and `EU`.
Building a country outside that set does not enlarge it. A published set that changes according to
what a program has already asked for is not a property of the library.

### (c)-42 — What the construction closure does not cover

Row 42 closes the construction of every **closed** type, and closes the roots of the closed
hierarchies as well. Two things are deliberately left open, and are open in the AAP as well as
here, so they are stated rather than implied.

`Index`, `RateIndex` and `FloatingRateIndex` are **not** among them. They were
sealed traits, which compile to plain JVM interfaces that any class file may implement without
running a constructor, so a foreign implementation could be accepted where the type is accepted and
an exhaustive match over it — one the compiler proved complete — would meet a shape the source does
not contain. AAP §0.3.3 makes the `Index` hierarchy closed, and closed means closed in the class
file too, so all three are abstract classes with package-private constructors whose bodies run
`JvmClosure.requirePermittedSubtype`; the gate compiles a foreign subtype of each level and none
constructs. `IndexObservation` stays an open trait, for the reason row 46 records, and
`FloatingRate` remains an open trait, which the AAP requires explicitly — it is kept
open so that `FloatingRateName` need not move into `Index.scala`. Both are listed below with the
other extension points.
- **The published extension points.** `IndexObservation`, `FloatingRate`, `ReferenceData`, `ReferenceDataId`,
  `DateAdjuster`, `FxRateProvider`, `FxConvertible`, `Resolvable`, `ResolvableCalculationTarget`,
  `CalculationTarget` and `TypedStringCompanion` are meant to be implemented by callers — that is
  what lets an application supply its own holidays, its own rates and its own instruments — and
  `ApiSurfaceSpec` asserts that each of them still admits an implementation, which is also the
  sensitivity control for every sealing assertion beside it.
- **The compiler's own `Serializable` classes.** The singleton class of every companion object, and
  the anonymous classes circe's derivation and Scala's lambdas produce, declare
  `java.io.Serializable` because their supertypes do. None is a product, none holds data of this
  library, and a module deserializes to the singleton it already is. The gate counts them (105 and
  155 on this build) and requires the remainder — a class of these modules holding data and taking
  part in Java serialization — to be empty.

### (c)-44 — Shape of the fail-fast checks

`ArgCheck` carries the checks of Java's `ArgChecker` that survive the port — the `notNull` family
does not, for the reason in (c)-3, and the preconditions whose exception type moved are row 11 —
with a different calling shape that is visible at every call site. Each surviving check keeps its Java message
character for character (row 33), with one exception, created by the collapse of two members into
one and stated in the table below:

| Java `ArgChecker` | `ArgCheck` | Why |
|---|---|---|
| A value check returns the checked argument — `notNull`, `notEmpty`, `notNegative` and the rest — so a check can be written inline in a field assignment; `isTrue`/`isFalse` return `void` | Every check returns `Unit` and is called in statement position | A discarded value is a compile error in this build, so returning the argument would force every call into a binding that exists only to be thrown away. The argument is already in scope at the call site, and validated construction goes through a factory that returns the finished value |
| Five `isTrue` forms — one without a message, one with a plain message, and three taking a message template with `Object...`, `long` or `double` arguments — and two `isFalse` forms, both with a message | One `isTrue(Boolean)`, one `isTrue(Boolean, => String)` and one `isFalse(Boolean, => String)` | A by-name message is built only when the check fails, and interpolation at the call site is checked by the compiler where a template is not. As in Java, there is no `isFalse` without a message |
| Separate emptiness checks for a collection (`Argument collection '<name>' must not be empty`, `ArgChecker.java:597`) and for an iterable (`Argument iterable '<name>' must not be empty`) | One `notEmpty(Iterable[T], String)` reporting `Argument iterable '<name>' must not be empty` (`ArgCheck.scala:382`). **This is the one message the port changes:** an empty collection is now reported with the iterable wording | `Iterable` covers every ordinary Scala collection, sequences, sets and maps alike; Java needed two members only because its collection type adds an emptiness test of its own. Nothing in either module asserts the collection wording |
| `notNull`, `noNulls`, `notNullItem` | Not ported — see (c)-3 and section (a) row 10 | Absence is `Option`, and a reference is never empty of a referent |
| The throw is the standard argument exception, raised directly | The same exception, raised directly through the one private `fail` of the object rather than through the standard `require` | `require` prefixes the message it is handed, which would change text that reaches logs and test expectations |

### (c)-45 — A failure carries no stack trace and no cause type

Java's `FailureItem` holds five fields: the reason, the message, the attributes, a **stack trace**
and an optional **cause type**. The trace is neither optional nor lazy — every factory builds one at
construction — but it does not always come from the same place:

- a failure built from a message alone synthesizes it from the current thread, walking
  `Thread.currentThread().getStackTrace()` and rendering the frames into a string with the message as
  the first line (`FailureItem.java:151,157-164`), and records no cause type;
- a failure built from a `Throwable` takes **that throwable's** trace instead
  (`Throwables.getStackTraceAsString(cause)`) and records the throwable's class as the cause type
  (`FailureItem.java:211-212`);
- a failure built from a `Throwable` that is itself a `FailureItemProvider` wraps the underlying item
  and inherits its trace and its cause type unchanged (`FailureItem.java:272`).

`Failure` holds three: the reason, the message and the attributes. Neither the trace nor the cause
type is carried, so neither appears in `Failure.show`, in the text form of any member, or in the JSON
form of section (e), and constructing a failure costs no stack walk. A failure here describes what
went wrong with the data, which is what a caller acts on and what a reader reads; where the frames
that produced it are wanted, they belong to the `IO` edge that escalates the failure, which raises an
ordinary error at the point of escalation.

### (c)-47 — Bounded calendar walks

Two operations of the calendar family are walks whose length is decided by something other than the
calendar: shifting a date by a number of business days walks that number of them, and finding the
next or previous business day walks closed days until it finds one. Java bounds neither. `shift`
loops `next`/`previous` over the count it was handed (`HolidayCalendar.java:97-109`),
`ImmutableHolidayCalendar` walks its own stored months instead
(`ImmutableHolidayCalendar.java:419-433`), the default `next`/`previous` step one day at a time
(`HolidayCalendar.java:120-123,150-153`), and no limit constant appears in either source.

**The count of a shift — `HolidayCalendar.MaxBusinessDayShift`, 100 000.** More than three hundred
years of business days. It is checked before the walk begins, and in more than one place, because a
count reaches a calendar by more than one route:

- `HolidayCalendar.shift` and `adjustBy` check it for every calendar, and
  `ImmutableHolidayCalendar` — the one implementation that overrides `shift` to answer from its
  stored months without reaching the inherited method — applies the same check, so the limit holds
  of a calendar holding data and not only of those holding none.
- `DaysAdjustment` judges the same figure where an adjustment is **built** rather than where it is
  applied. An adjustment is data: it is read from a document, held in a convention and passed
  around, so a count of two thousand million would otherwise arrive from outside the program and
  occupy a processor every time the adjustment was applied. Judging it at construction is what makes
  every adjustment that exists one that can be applied. `DaysAdjustment.of`, which has a failure
  channel, reports it; the named business-day factories, which are total as the Java ones were,
  raise it (row 2).
- A **calendar-day** addition is exempt, and so is the no-holidays calendar, because such an
  addition is arithmetic on the date in one step rather than a walk. `DaysAdjustment.ofCalendarDays`
  is therefore total for every count an `Int` holds, exactly as the factory being ported was.

Both checks measure the magnitude in `Long` arithmetic, because the magnitude of the smallest `Int`
is not an `Int`: negating it overflows back to itself, and an `Int` comparison would pass.

**The closure of a calendar — `HolidayCalendar.MaxConsecutiveHolidays`, 3 653.** Ten years. The
search for a business day is required to make progress: it crosses at most this many days and is
refused beyond that, naming the calendar and the date it began at. The figure is what separates a
calendar with no business day from one merely shut for a long time — no centre closes for ten years,
and a calendar built from published holidays never states such a run — and the refusal is reachable
only by a calendar that has **none**:

- a weekend that closes all seven days of the week. Java built such a calendar — its `of` applies
  no check to the weekend days it is handed — while here no factory of `ImmutableHolidayCalendar`
  builds one and no document decodes to one: the refusal is a caller-contract `ArgCheck` stated
  where a calendar comes into being, as the year-range precondition beside it is, and the decoder
  states it in its own channel. So this case is refused earlier than the walk, and with a message
  that names the weekend rather than the walk.
- a composite whose parts between them close every day — `Sat/Sun` read together with a calendar
  whose weekend is Monday to Friday, which is two perfectly ordinary calendars. This one cannot be
  refused at construction, since neither part is defective, which is why the search itself carries
  the bound and not only the factory.

Without the bound such a search would walk millions of days to the end of the range of dates a
calendar can answer about and then report the year it reached, which says nothing about what is
actually wrong. Refusing it names the calendar and the date instead. The cost is one comparison per
day crossed, and the message is built only where the search is refused.

### (c)-48 — Bounded calendar composition

Composing calendars builds a tree, and every recursive operation of the family walks it: deciding
whether a date is a holiday, composing the identifier, writing the calendar out, reading one back.
Java composes without limit, and decomposes a composite **name** without limit as well — `of`
splits on the separators, allocates an identifier per part, de-duplicates them, sorts them and joins
them back (`HolidayCalendarId.java:87-120`). Neither source carries a ceiling.

**Depth — `HolidayCalendar.MaxCompositeDepth`, 128.** A composite reads two calendars, either of
which may be a composite of its own, so a tree tall enough exhausts the stack, which ends the
calling thread rather than the calculation. The height of a composite is one more than the taller of
its parts and is computed from heights the parts already hold, so the check costs one comparison
however tall the tree is and cannot be outrun by building it from the bottom up. It is applied at
every point a composite comes into being: `combinedWith` and `linkedWith`, the constructors of
`Combined` and `Linked` themselves, the reading of a calendar from a document, and
`HolidayCalendarId.resolve` — which reads the same predicate rather than the raising check, because
it combines calendars a caller supplied and answers in a failure channel, so resolving an identifier
still answers rather than raising. The figure is generous by a factor of four over anything real: a
payment settling in every centre this library knows about reads some thirty calendars.

**Name — `MaxCompositeNameLength`, 65 536 characters, and `MaxCompositeParts`, 4 096 parts.**
`HolidayCalendarId.of` is total and stays total: it is reached from inside `Either` and from circe
decoding, so a name it refused would put a failure outside the channel those callers read. What is
bounded is therefore the work, not the input. A name past either ceiling is kept as a **simple,
non-composite identifier of exactly the text it was given**, rather than being decomposed, sorted
and rejoined, and the consequence is a behaviour and not only a cost:

- it is not normalised, so it is equal only to itself and to an identically spelled name — where a
  name within the ceilings is equal to every permutation of its parts;
- it reports itself as not composite;
- it resolves against reference data only if a host supplied that exact name, which for such a name
  means against nothing.

The part count is one scan over the characters that allocates nothing and stops the moment the
ceiling is passed, so reading a name never costs more than reading it, and a name written to hold a
million separators is abandoned after the first few thousand. Every composite an application writes
is orders of magnitude inside both figures — a composite of every calendar this library knows is
under two hundred characters — including the ten-thousand-character name of two thousand and one
calendars that the day count grammar of this library admits, which still resolves to `GBLO+USNY`.

### (c)-49 — Bounded parse text

Java places no length bound on the text any of these types is parsed from: `StandardId.of` states
the permitted lengths of its scheme and value as `1, Integer.MAX_VALUE`
(`StandardId.java:165-166`), `CurrencyAmount.parse` tests only that the text is long enough
(`CurrencyAmount.java:120`), and `FxRate.parse`, `Tenor.parse`, `MarketTenor.parse` and
`Frequency.parse` test no length at all. Each of them then scans, folds or copies the whole of what
it was handed, so the work of a refusal is proportional to text whose length the sender chose
(CWE-400/CWE-770).

Each states a ceiling here, tested **before** any of that work:

| Member | Ceiling | What it admits, and what it refuses |
|---|---|---|
| `StandardId.of` — scheme and value | `MaxPartLength` = 65 536 characters, each part | Identifiers name instruments, schemes, tickers and exchange codes, tens of characters each; the largest the suite exercises is ten thousand, asserted there to be a legal identifier. This is the one ceiling in this row that refuses an identifier no shorter text also names: past it, a value Java accepted is refused for its size alone |
| `StandardId.parse` — the whole text | `MaxTextLength` = `2 * MaxPartLength + 1` = 131 073 | Derived rather than chosen. Two admitted parts render as `scheme~value`, so this is exactly the longest text any value of the type can render to, and `parse` of the rendering of any value the factories accept is still that value — at every size up to the largest |
| `CurrencyAmount.parse` | `MaxAmountTextLength` = 1 024 | The longest exact decimal spelling of a finite double is that of the smallest subnormal, 767 significant digits, and every other value needs fewer, so no spelling that names a double exactly is refused. What is refused is text carrying digits that cannot change the value it names |
| `FxRate.parse` | `MaxRateTextLength` = 1 024 | The same reasoning, and the same figure. Text past it is reported with the wording of a rate that could not be read rather than one of its own, so the ten-thousand-digit rate of the test suite — refused here for its size where it used to be refused for being zero — still reads as a rate that names no legal value |
| `Tenor.parse`, `MarketTenor.parse`, `Frequency.parse` | `MaxTextLength` = 256 each | The longest text that can name a tenor at all is a signed count of years, months, weeks and days, which is under fifty characters even with every count written out to the ten digits an `Int` holds — so the ceiling is four times the longest text that can succeed, and what it refuses is padding: a zero-padded or otherwise inflated spelling of a period that a much shorter text also names. The figure is the one `Decimal` already applies to the numeral it reads, which is why the text ceilings of these grammars are one number |

Three figures in this area are **Java's own**, restated rather than added, and are recorded here so
that a reader meeting them does not take them for new refusals:

- `Money` and `BigMoney` bound the amount text at 256 characters. That is the bound Java's `Decimal`
  already applied to the numeral it reads (`Decimal.java:126-127`, `Decimal string must not exceed
  256 characters`), restated so that it can be applied before the numeral is copied out of the text
  rather than after. Nothing about what is accepted changes and the wording is the same either way.
- `CurrencyPair.parse` tests its text against `PairTextLength`, the 7 characters its expression
  fixes. The test is an **upper bound** rather than an equality, and it refuses nothing: case folding never
  produces fewer characters than it was given, so text longer than seven could not have matched
  `([A-Z]{3})/([A-Z]{3})` however it was folded — while folding a million characters in order to
  discard the result is work the bound removes. Folding can make text *longer*, so a six-character
  text is still folded and matched as it always was.
- `Frequency`'s `MaxYears` of 1 000 is Java's `MAX_YEARS` (`Frequency.java:68`), refused with Java's
  own message (`Years must not exceed 1,000`, `Frequency.java:292-303`). It bounds the length a
  frequency may state, not the text it is read from.

### (c)-52 — A run of amounts holds no NaN

`CurrencyAmount` rejects a NaN on both sides (`CurrencyAmount.java:148`), but the two array types
that hold runs of amounts examine no element: no NaN check appears in either Java source, so a run
could hold a value that is no amount, and every later reader of that run would fail on it at a
distance from where it was built.

Here the elements of a run are amounts kept as numbers, and that is an invariant of the type:

- The examination is stated once, at the construction point every route passes through, so no
  factory goes round it, and its message is taken by name so the accepted path allocates nothing for
  it.
- The three members that are **total in signature** therefore raise: `of(currency, DoubleArray)`,
  which AAP §0.4.1 requires to be total, and `multipliedBy` and `mapAmounts`, which were total in
  the type being ported. `multipliedBy` reaches it for a factor of zero applied to an infinite
  element or a factor that is itself not a number; `mapAmounts` for a mapper that produces such a
  value. The refusal happens where the value was produced rather than later where something read it
  back, and it is a fail-fast `ArgCheck` throw naming the index (row 2).
- Every route that has a failure channel of its own performs the same examination first and
  **reports** it rather than raising: the decoder, the four members that add or subtract, and the
  two conversions — this type's own and the one `MultiCurrencyAmountArray` performs when it
  collapses its currencies into a run of this type. So no caller reading an `Either` meets the throw.
- `of(Iterable[CurrencyAmount])` and `of(size, fn)` need no examination of their own: every element
  they are given is already a `CurrencyAmount`, and `CurrencyAmount` rejects a NaN at its own
  construction, so the invariant holds of what they build by construction.
- The invariant is restated on the implementation class with `JvmClosure.requireInvariant`, over the
  field the instance actually holds, because that class carries a public constructor in the class
  file whatever the source asked for (row 42). A run forged through it is refused in the same
  constructor.
- `MultiCurrencyAmountArray` does the same, with the same split between raising and reporting, and
  states the element invariant beside its two structural ones — that its size is not negative, and
  that it holds exactly one value per index of the run for each of its currencies. Its scan is
  per currency, in the order the run holds them, and it is an iterator rather than a collection so
  that the raise reads only the first offending entry while the accumulating route reads all of
  them.

Two edges are deliberate. The two infinities **are** values an amount holds, on both sides, and are
admitted. The empty run holds no element and so satisfies the invariant vacuously, which is what a
factory total in signature requires. The scan is `DoubleArray.indexOf`, one pass over the primitive
array comparing bit patterns — which finds a not-a-number value however it arose, where an ordinary
comparison finds none — and nothing is boxed or allocated by it.

## (d) Covered holiday calendar set

The Java module generated its standard calendars from rules and cached them in
`GlobalHolidayCalendars.bin`; the port keeps the rules as pure Scala functions, memoised as
`lazy val`s, and carries no cache (divergence (c)-31). The built-in set is the same 30 values the Java
registry published, and `ReferenceData.standard` is exactly that set keyed by `HolidayCalendarId`.

**Generated from rules, 1950–2099 (24 calendars):** `AUSY` `BRBD` `CAMO` `CATO` `CHZU` `CZPR` `DEFR`
`DKCO` `FRPA` `GBLO` `HUBU` `JPTO` `MXMC` `NOOS` `NYFD` `NYSE` `NZAU` `NZBD` `NZWE` `PLWA` `SEST`
`USGS` `USNY` `ZAJO`.

**Generated with its own range:** `EUTA`, over 1997–2099, and over that range it is not one rule but
three, reproduced from the Java generator exactly:

- **1997–1999** — January 1 and December 25 only, the years in which the system was in its testing
  phase.
- **1999 and 2001** — December 31 in addition.
- **2000 onwards** — the full rule: January 1, Good Friday, Easter Monday, May 1, December 25,
  December 26.

A weekend date is dropped from all three, as for every other generated calendar. AAP §0.6.2
summarises this generator as looping 1997–2099 and "adds holidays only from 2000 onwards", which
describes the third rule and not the first two; the port follows the Java generator the same sentence
cites (`GlobalHolidayCalendars.java:294-313`), because Rule 2 requires the holiday sets to match Java
and the captured Java baseline carries the pre-2000 dates — `holiday-baseline.json` holds
`1997-01-01`, `1997-12-25`, `1998-12-25` and `1999-12-31` for `EUTA`, each of them a weekday, so each
can only be there by the rule, and `HolidayCalendarParitySpec` asserts the year's whole set. Anyone
reconciling the summary with the code should read the generator and that fixture as the measured
authority for this calendar.

**Explicit date table:** `THBA`, 75 year-entries spanning 2005–2079, transcribed from the Java
`HolidayCalendarData.ini` rows.

**Weekend and no-holiday calendars:** `NoHolidays`, `Sat/Sun`, `Fri/Sat`, `Thu/Fri`. These four are
also exactly `ReferenceData.minimal`, and each of the three weekend calendars carries its own
identifier, as in Java.

Behaviour worth knowing about the set:

- **Out-of-range dates fall back to a weekend-only test**, above and below each calendar's stored
  range, exactly as Java did — so a 1949 or 2100 date against `GBLO` is a business day unless it is a
  weekend, and years outside 0–9999 are rejected (divergence (c)-2).
- **`HUBU` represents Saturdays explicitly.** It declares Sunday as its only weekend day and adds
  every non-working Saturday as a holiday, so its `weekendDays` is `{SUNDAY}` and its working
  Saturdays are visible as business days — the Java arrangement, preserved.
- **Working-day overrides are applied last** in the four-argument `ImmutableHolidayCalendar.of`,
  overriding both holidays and weekend days for the dates they name, and overrides outside the
  derived year range are ignored — again as in Java. No built-in calendar uses them; they served the
  INI `WorkingDays` key and remain part of the public factory.
- **Composite identifiers** are supported for any combination: `+` combines (a business day only
  where both agree), `~` links (a holiday only where both agree), component names are normalised,
  sorted and deduplicated, and `NoHolidays` is absorbed on `+`. Resolution tries the whole name first
  — see divergence (c)-20 for the one place the port is more permissive than Java.
- **13 default-by-currency identifiers have no built-in calendar**, in the port exactly as in Java:
  `CLSA` `CNBE` `COBO` `HKHK` `IDJA` `ILTA` `INMU` `KRSE` `RUMO` `SARI` `SGSI` `TRIS` `TWTA`. They are
  valid identifiers that fail to resolve against `ReferenceData.standard` (`Failure.MissingData`); a
  host supplies them. `HolidayCalendarId.defaultByCurrency` answers `None` for the three currencies
  Java threw for (`PHP`, `MYR`, `XAU`).
- **`HolidaySafeReferenceData`** behaves as in Java: an unknown *non-composite* identifier resolves to
  a weekend-only calendar carrying the **requested** identifier, an unknown composite one is left for
  component resolution, and `containsValue` is `true` for every `HolidayCalendarId`.

Every generated calendar was compared with the Java implementation year by year across its full range
plus one year either side, on the holiday list, the business-day count and the weekend-day set; the
`holiday-baseline.json` fixture carries the captured Java expectations.

## (e) JSON shapes and exclusions

Serialization is circe, derived at compile time — `io.circe.generic.semiauto` for products, hand-written
codecs where a shape needs one — with no reflection anywhere on the codec path. The helpers the rules
below are built from live in
`strata-collect/src/main/scala/com/opengamma/strata/collect/json/Codecs.scala`.

**General rules.** Each rule states the output shape, which is what a consumer depends on, and then
the mechanism that produces it. The shapes are uniform across a group; the mechanisms are not, so
they are named individually.

- **Named types are bare strings.** Every `Named` family encodes as its canonical name and decodes
  through its own `parse`, so the JSON of a day count, convention, index, currency or calendar
  identifier is the same string Joda-Convert produced: `"GBP"`, `"Following"`, `"GBLO+USNY"`, `"P3M"`,
  `"3M"`. Most families take that shape from `Codecs.namedEnumCodec`; `FailureReason` states it
  directly (`Encoder.encodeString.contramap(_.name)` with `Decoder.decodeString.emap(parse)`), and
  `FloatingRateName` takes it from `Codecs.parsedStringCodec`, whose parse reports a single cause
  rather than a chain.
- **Open string-typed values** encode and decode as one string, printing what `toString` prints:
  `CurrencyPair`, `HolidayCalendarId`, `Tenor`, `MarketTenor`, `Frequency`, `StandardId` and `Decimal`
  through `Codecs.parsedStringCodec`, `Country` and `FixedScaleDecimal` through
  `Codecs.parsedStringCodecNec`, which differs only in taking a constructor that accumulates its
  failures into an `EitherNec`. The shape is identical either way.
- **Products use Java property names as keys**, in declaration order, and an absent optional field is
  omitted rather than written as `null`. The wrapper that does it is applied as a policy and not only
  where it bites: **27** product encoders wrap `Codecs.dropNulls`, and for those of them with no
  optional field the wrapper is a deliberate no-op rather than a sign that one exists —
  `CurrencyAmount` is one such, and its encoder says so. **Three** products encode through their
  derived encoder with no wrapper: `ValueAdjustment`, `ValueDerivatives` and `Failure`. **Three**
  types are written by hand instead of derived: `DayCount`, `HolidayCalendar` and `Rounding`. Those
  33 encoders are all of them: every other codec-bearing type is a bare string by the two rules
  above, bar `DoubleArray` and `DoubleMatrix`, whose array shapes `Codecs` supplies directly.
- **Every `Double` is tagged.** A finite value is a JSON number; `NaN`, `Infinity` and `-Infinity` are
  the strings `"NaN"`, `"Infinity"`, `"-Infinity"`. The decoder accepts a number or one of those three
  strings, and refuses a number that is not finite (divergence (c)-29). Whether a non-finite value is
  *valid* is the type's decision, not the codec's — `CurrencyAmount` rejects `NaN` and accepts the
  infinities, as Java did.
- **Sealed families use the wrapper object** keyed by the constructor name — `{"HalfUp":{…}}`,
  `{"MissingData":{…}}` — rather than a discriminator field, so no `circe-generic-extras` dependency
  is needed and none is declared: `build.sbt` asks for `circe-core`, `circe-generic` and
  `circe-parser` and nothing else. `Failure` derives that wrapper; `Rounding` builds it by hand with
  `Encoder.instance`, and `DayCount` and `HolidayCalendar` are hand-written throughout, because each
  of those three mixes forms that no single derivation produces.
- **Decoding is where a type's own rules are applied**, so a payload that violates an invariant fails
  with a `DecodingFailure` carrying the joined failure messages rather than producing an invalid
  value. Every product decoder takes one of four routes, and these are all of them. (1)
  `Codecs.validatedDecoder`, which feeds a derived decoder for the constructor fields into an `of`
  that accumulates into an `EitherNec` — **13** types: `FxRate`, `FxMatrix`, `CurrencyAmountArray`,
  `MultiCurrencyAmountArray`, `AdjustableDates`, `SequenceDate`, `PeriodAdjustment`,
  `TenorAdjustment`, `SchedulePeriod`, `Schedule`, `PeriodicSchedule`, `ValueStep`,
  `ValueStepSequence` — and, inside the hand-written `Rounding` decoder, its `HalfUp` branch. (2)
  `Codecs.checkedDecoder`, the same thing over an `of` that reports a single cause in an `Either` —
  **5** types: `CurrencyAmount`, `MultiCurrencyAmount`, `IborIndexObservation`,
  `OvernightIndexObservation`, `FxIndexObservation`. (3) A derived raw decoder mapped through a
  **total** factory, where construction cannot fail but still normalises — `Money`, `BigMoney`,
  `DaysAdjustment`, `ValueSchedule`. (4) A plain derived decoder, for the products that are total in
  every field — `AdjustableDate`, `AdjustablePayment`, `BusinessDayAdjustment`, `Payment`,
  `PriceIndexObservation`, `ValueAdjustment`, `ValueDerivatives`, `Failure`. Encoders stay derived,
  with `dropNulls` per the rule above, because an in-memory value is already valid.
- **Byte stability.** A document is a function of the value alone, so re-encoding what was decoded
  reproduces the same bytes — `JsonRoundTripSpec` asserts that of every codec-bearing type. Where the
  order fields were supplied in could otherwise show, the value is ordered: map-like fields are
  `SortedMap`/`SortedSet` (`Failure.attributes`, multi-currency amounts by currency code, holiday sets
  by date), product fields encode in declaration order, and dates and days of the week inside a
  calendar document are written ascending; `FxMatrix` instead carries its currency order *as part of
  the value*, so two matrices built in different orders are different values and encode differently.
  All of that is a statement about **data, not about equality**. `HolidayCalendar` equality is by
  identifier, while the codec chooses its form by identity against the built-in set, so the library's
  `GBLO` and an application's own calendar carrying the identifier `GBLO` are equal values that encode
  differently — the identifier string and the structural form respectively — which is exactly what
  stops the second being read back as the first.
- **`java.time` values** use circe's ISO-8601 codecs — `2024-01-31`, `Europe/London`, `P3M`,
  `2024-01` — with two additions: `DayOfWeek` encodes as the enum constant name (`"SATURDAY"`), and
  `LocalTime` uses the port's own encoder so a time renders `11:00` rather than `11:00:00`
  (divergence (c)-29).

**Shapes**

The 58 codec-bearing types, grouped by the shape they take. `JsonRoundTripSpec` round-trips every one
of them through `Arbitraries` and prints the covered list beside the excluded list below, which is
what Gate 4 compares against this section.

| Type | Shape |
|---|---|
| `Currency`, `BusinessDayConvention`, `RollConvention` (all 45 values, `Day15` and `DayMon` included), `PeriodAdditionConvention`, `DateSequence`, `StubConvention`, `FloatingRateType`, `ValueAdjustmentType`, `IborIndex`, `OvernightIndex`, `PriceIndex`, `FxIndex` | Bare canonical-name string, through `Codecs.namedEnumCodec` — the same string Joda-Convert wrote |
| `FailureReason`, `FloatingRateName` | The same bare canonical-name string, built without `namedEnumCodec`: `FailureReason` from `Encoder.encodeString.contramap(_.name)` and `Decoder.decodeString.emap(parse)`, `FloatingRateName` from `Codecs.parsedStringCodec` |
| `CurrencyPair`, `HolidayCalendarId`, `Tenor`, `MarketTenor`, `Frequency`, `StandardId`, `Decimal` | Bare string in the Java `toString` form (`"EUR/USD"`, `"GBLO+USNY"`, `"3M"`, `"ON"`, `"P3M"`, `"scheme~value"`), through `Codecs.parsedStringCodec` |
| `Country`, `FixedScaleDecimal` | The same bare-string shape (`"GB"`, `"1.50"`), through `Codecs.parsedStringCodecNec`, whose constructor accumulates its failures |
| `DayCount` | Hand-written. A standard member is its name string (`"Act/365F"`); `Bus252` is structural, so a custom calendar survives the trip — `{"Bus252":{"name":"Bus/252 BRBD","calendar":"BRBD"}}`, the `calendar` field being a whole `HolidayCalendar` document. The decoder takes either form, resolving a `Bus/252 X` string against the built-in set and checking that a structural `name` equals `"Bus/252 " + calendar.id` |
| `HolidayCalendar` | Hand-written. A built-in value encodes as its identifier string; any other value as a wrapper object — `{"Immutable":{"id":"XCAL","weekendDays":["SATURDAY","SUNDAY"],"startYear":2020,"holidays":["2020-01-01"],"workingWeekendDays":[]}}`, `{"Combined":{"a":"GBLO","b":"USNY"}}`, `{"Linked":{"a":…,"b":…}}`. The internal bitmask never appears. See divergence (c)-24 for what a round trip preserves |
| `CurrencyAmount` | `{"currency":"GBP","amount":100.0}`, `amount` tagged |
| `Money`, `BigMoney` | `{"currency":"GBP","amount":"12.34"}` — the amount is a `Decimal`, so it takes the `Decimal` string form |
| `MultiCurrencyAmount` | `{"amounts":[{"currency":"EUR","amount":500000.0},{"currency":"GBP","amount":1000000.0}]}`, sorted by currency code and decoded through `of`, so a duplicate currency is a `DecodingFailure` |
| `CurrencyAmountArray` | `{"currency":"GBP","values":[1.0,2.0,"Infinity"]}` |
| `MultiCurrencyAmountArray` | `{"size":2,"values":{"GBP":[1.0,0.0],"USD":[0.0,"Infinity"]}}` |
| `FxRate` | `{"pair":"EUR/USD","rate":1.25}` |
| `FxMatrix` | `{"currencies":["GBP","USD"],"rates":[[1.0,1.6],[0.625,1.0]]}` — currencies in the matrix's own insertion order, rates tagged, decoded through `fromMatrix`, which checks unique currencies, a square matrix of matching size and a unit diagonal |
| `Payment`, `AdjustablePayment` | `{"value":{"currency":"GBP","amount":1000.0},"date":"2015-06-30"}`, the adjustable form carrying an `AdjustableDate` |
| `AdjustableDate`, `AdjustableDates` | `{"unadjusted":"2024-01-31","adjustment":{"convention":"Following","calendar":"GBLO"}}`, the plural form over a non-empty date list |
| `BusinessDayAdjustment` | `{"convention":"Following","calendar":"GBLO"}` |
| `DaysAdjustment`, `PeriodAdjustment`, `TenorAdjustment` | Product forms over the offset, its convention and its calendars; absent fields omitted |
| `SequenceDate` | Product form, absent fields omitted (divergence (c)-28) |
| `PeriodicSchedule`, `SchedulePeriod`, `Schedule` | Product forms with the Java property names; a `Schedule` is `{"periods":[{"startDate":…,"endDate":…,"unadjustedStartDate":…,"unadjustedEndDate":…}, …],"frequency":"P3M","rollConvention":"Day25"}` |
| `IborIndexObservation`, `OvernightIndexObservation`, `FxIndexObservation` | Product forms whose `index` is the name string and whose derived dates are explicit — `{"index":"GBP/USD-WM","fixingDate":"2016-02-22","maturityDate":"2016-02-24"}` — decoded through `of(index, fixingDate, ReferenceData.standard)` and checked against the payload, so an inconsistent document fails |
| `PriceIndexObservation` | `{"index":"GB-RPI","fixingMonth":"2024-01"}` |
| `Rounding` | `{"NoRounding":{}}` / `{"HalfUp":{"decimalPlaces":2,"fraction":0}}`; an unknown field is rejected |
| `ValueAdjustment`, `ValueDerivatives` | Product forms; `ValueDerivatives.derivatives` is a tagged-double array |
| `ValueStep`, `ValueSchedule`, `ValueStepSequence` | Product forms; a step is `{"periodIndex":2,"value":{"modifyingValue":-2000.0,"type":"DeltaAmount"}}` or the `date`-keyed alternative, exactly one of the two being present |
| `DoubleArray`, `DoubleMatrix` | A JSON array of tagged doubles, and an array of row arrays; both bounded on decode. Supplied by `Codecs.doubleArrayCodec`/`doubleMatrixCodec` rather than as companion implicits, so a caller opts in |
| `Failure` | `{"MissingData":{"message":"…","attributes":{…}}}`, attributes sorted |
| `Currency`, `HolidayCalendarId` as map **keys** | The same bare strings, through `Codecs.namedKeyCodecs` |

**Excluded from JSON, with the reason**

| Type | Why it has no codec |
|---|---|
| `ReferenceData`, `ImmutableReferenceData`, `CombinedReferenceData`, `HolidaySafeReferenceData` | A heterogeneous identifier-to-value store. Only holiday calendars in it are serializable, and they are carried by the `HolidayCalendar` codec |
| `ReferenceData.Entry[T]` | Same reason — one entry of that store |
| `ReferenceDataId[T]` other than `HolidayCalendarId` | A behavioural abstraction, not data |
| `DayCount.ScheduleInfo` | A behavioural interface; its one real implementation, `Schedule`, has a codec |
| `DateAdjuster`, `FxRateProvider`, `LazyFxRateProvider`, `FxConvertible`, `Resolvable`, `ResolvableCalculationTarget`, `CalculationTarget` and therefore `CalculationTargetList` | Function and contract types with no data of their own. `LazyFxRateProvider` is a `FxRateProvider` that defers to a `lazy val`, so it has less data still |
| `Matrix` | A trait; `DoubleMatrix` is covered |
| `Named`, `NamedEnum`, `TypedStringCompanion`, `ArgCheck`, `Validate`, `Collections`, `DoubleArrayMath`, `Resources`, `Codecs` | Typeclasses, helpers and effects, not data |
| `FailureOr`, `ResultNec`, `ValidatedFailures`, `ValueWithFailures` | Generic containers — circe's `Either`/`Validated`/`Ior` instances apply once `Failure` and the value type have codecs |
| `Index`, `RateIndex`, `FloatingRateIndex`, `FloatingRate`, `IndexObservation` | Traits; their leaf families carry the codecs |

No Joda-Beans JSON or binary form is read or written, and no type supports Java serialization
(divergence (c)-30).

## (f) Demo command and gate script

Everything is driven by sbt from the repository root, on JDK 21. The build defines exactly two
projects, `strata-basics` (the root project, whose sources live under `strata-basics/`) and
`strata-collect`, with `strata-basics` depending on `strata-collect` for both compile and test.

```
sbt -batch clean compile Test/compile     # build both modules under -release 21 -Werror
sbt -batch test                           # run both modules' suites through aggregation
sbt -batch "strata-basics/run"            # the end-to-end demo (see below)
scripts/verify-gates.sh                   # the single automated acceptance gate runner
```

Always pass `-batch`: a bare `sbt` shell does not return. On a small host, cap the heap through
`SBT_OPTS`, which `sbt` reads from its environment — so either export it for the shell,
`export SBT_OPTS="-Xmx1200m -Xss8m -XX:MaxMetaspaceSize=512m"`, or prefix the one command,
`SBT_OPTS="-Xmx1200m -Xss8m -XX:MaxMetaspaceSize=512m" sbt -batch test`. A bare assignment on its own
line is not inherited by the child process.

**The demo.** `sbt "strata-basics/run"` — `sbt -batch "strata-basics/run"` in a script — runs
`com.opengamma.strata.basics.demo.BasicsDemoApp`, the `IOApp.Simple` registered as the root project's
`mainClass`. It performs the four steps of the deliverable in order, and the values below are the run
in this tree:

1. **Builds a `PeriodicSchedule`** — 2024-03-25 to 2025-03-25, `Frequency.P3M`, `ModifiedFollowing`
   over `HolidayCalendarIds.GBLO`, `StubConvention.NONE`.
2. **Adjusts its dates against a built-in `HolidayCalendar` through explicit `ReferenceData`** —
   `createSchedule(ReferenceData.standard)` yields four periods with the implied roll convention
   `Day25`, two of whose dates `GBLO` moves (2024-12-25 → 2024-12-27).
3. **Converts a `MultiCurrencyAmount` via FX** — `[EUR 500000, GBP 1000000]` through an `FxMatrix`
   built from `GBP/USD 1.27` and `EUR/USD 1.09`, giving `USD 1815000`.
4. **Serializes to JSON and prints** — the `Schedule`, the `MultiCurrencyAmount` and the converted
   `CurrencyAmount`, each through its own circe codec:
   `{"currency":"USD","amount":1815000.0}`.

Every `Either` the demo meets is lifted at the `IO` edge by its own private `raise` helper, which is
the only place in `strata-basics` that turns a `Failure` into a throwable. Nothing else is needed to
run it — no service, no database, no browser.

**The gate script.** `scripts/verify-gates.sh` is the one authoritative runner: from a clean checkout
it executes every automated gate in order, writes `target/gate-report.md` together with the aggregated
parity counts from `target/parity-report/*.json`, the per-module test counts from
`target/test-reports/*.xml` and the codec-coverage list, and exits non-zero if any gate fails.

The rows it runs are below, under the script's own row names, so a row in `target/gate-report.md` and
a row here are the same thing. The specification numbers **gates** and **rules** separately: most rows
carry both identities, four carry only a rule number, and `Gate 5` is six rows rather than one.

| Gate / Rule — the script's row | What it establishes |
|---|---|
| `Gate 1 - builds and runs` | Both modules compile under `-release 21 -Werror` and every spec passes |
| `Gate 2 / Rule 1 - dependency purity` | No Guava, no Joda and no Java `strata-collect` jar on either module's compile or test classpath |
| `Gate 2a / Rule 1a - exactly two Scala-only modules` | Exactly the project ids `strata-basics` and `strata-collect`, zero `.java` files, and the `strata-basics → strata-collect` edge |
| `Gate 3 / Rule 2 - numerical parity` | Every parity spec green with zero failed rows, at 1e-9 absolute and relative |
| `Gate 4 - serialization round-trip` | Every codec-bearing type round-trips, and the encoding is byte-stable in the scope section (e) states |
| `Gate 5 / Rule 3 - no var in domain code` | No `var` in either module's main sources |
| `Gate 5 / Rule 3 - no boxing in the numeric hot paths` | No `BoxesRunTime` call and no `Double` boxing in the bytecode of the named array and matrix methods |
| `Gate 5 / Rule 5 - explicit error handling` | No `null` and no `throw new` outside `ArgCheck`, with the smart-constructor, failable-surface, API-surface and `Failure` specs green |
| `Gate 5 / Rule 7 - IO at the edges` | `cats.effect` appears only in `collect.io.Resources` and under `basics.demo` |
| `Gate 5 - typeclass instances` | The instance inventory `TypeclassLawsSpec` summons at compile time, with the cats law suites green |
| `Gate 5 / Rule 4 - closed enums and data fidelity` | Every family closed and every alias, external and lenient row resolving; every data table equal to the Java manifest; no resource lookup in main sources |
| `Rule 6 - no reflection on the codec path` | Proved by a class-load delta between an encode-and-decode run and a baseline run |
| `Rule 8 - JVM 21 bytecode` | Class-file major version 65 |
| `Rule 9 - warning-clean` | `-Werror` in the common settings, and no `@nowarn`, `@SuppressWarnings` or `-Wconf` anywhere |
| `Rule 10 - Scala collections in the public API` | No `java.util` collection, `Optional`, stream or function type in either module's public API, in source or in bytecode, with a test-scope negative control proving the matcher sees one when it is there |
| `Gate 6 - end-to-end demo` | `sbt -batch "strata-basics/run"` runs the four demo steps described above and exits 0 |
| `Gate 7 - migration note (automated)` | This note: present, its six sections present as `## ` headings, and section (a) carrying at least as many table rows as there are distinct `strata-collect` members referenced from `modules/basics/src` |
| `Gate 7 - migration note (manual approval)` | Reported, never run: an approving pull-request review by a `CODEOWNERS` owner. The script records the row and never blocks on a human |
| `Test scope >= Java` | Test scope at least equal to the Java suites, joined method by method through `java-test-mapping.csv` |
| `Repository boundary` | `git status --porcelain -- modules examples eclipse pom.xml src .github` is empty — the Maven tree and the repository's governance files are unchanged |

**Gate 7 is this note, and it has two halves.** The automated half is the
`Gate 7 - migration note (automated)` row: the script checks that `SCALA_MIGRATION.md` exists, that
its six sections are present as `## ` headings, and that section (a) carries at least as many table
rows as there are distinct `strata-collect` members referenced from `modules/basics/src` — a count
the script computes with the specification's own grep rather than hardcoding. Measured on this
revision: **51** referenced members against the **85** rows of section (a)'s required table, and all
six required sections present once each and in order. The row reports the total number of `## `
headings without judging it, so section (g)'s ledger is counted and never required. The script counts every table line inside section (a) and subtracts one header and one
separator, so it reads **108** rows from the 110 lines there - section (a) carries three tables, and
the eleven supplemental rows, the eight published-addition rows and the two later tables' own headers
and separators are all counted; the required table's 85 rows alone already clear the threshold. The manual half is an approving pull-request review by a
[`CODEOWNERS`](.github/CODEOWNERS) owner, confirming this note's content against the six items the
migration note owes; the script never blocks on a human, and reports that half as "automated checks
passed; manual approval: see PR review".

**The Maven tree is untouched, and deliberately so.** This port is *additive*: `build.sbt` adds two
Scala projects beside the existing Maven reactor and never references it, and every file under
`modules/**` is byte-identical to what it was. That tree is what later slices port their modules
from, and it is what the parity baselines and the reference-data manifest are regenerated from —
`tools/parity-capture/capture-baseline.jsh`, driven by the procedure in
[`tools/parity-capture/README.md`](tools/parity-capture/README.md), builds the Java jars with Maven
and re-emits the six fixtures and the manifest. The gate script asserts the tree's cleanliness
directly: `git status --porcelain -- modules examples eclipse pom.xml src .github` must be empty.

Two details of the test-scope row follow from divergence (c)-32, and the script honours both: it
reads `java-test-mapping.csv` with a quoting-aware CSV reader (`python3`'s `csv`) rather than
`awk -F,`, because 79 rows quote a test name containing a comma; and it permits `dropped` for the
five test classes section 0.2.2 excludes whole — and for nothing else, since a class-wide exception
for a class that keeps any method would let one `dropped` row retire a retained Java test method
without a Scala counterpart. There is no method-level exclusion to permit: the manifest's 18
`dropped` rows are all rows of those five classes, which is what the row's own evidence records
(`dropped rows: 18 across 5 wholly excluded classes`), and every method of every retained class —
`ImmutableHolidayCalendarTest.test_readOldJodaFormat` among them — names a Scala test case.

Forked tests write their parity reports to `target/parity-report` and their JUnit XML to
`target/test-reports`, both under the repository root, whichever project ran them, because
`build.sbt` hands both projects those two absolute paths.

## (g) Remediation ledger — build, CI and dependency-affecting findings

This section is not one of the six the AAP requires. It exists because the review of this port's
dependency safety and supply chain needed evidence the repository did not hold: the reconciled reports
of the build-configuration, CI and gate-runner reviews are review artefacts that live beside the run
rather than in the tree, so what they decided about dependencies could only be reconstructed from
commit messages. This ledger is the in-repository record of the same facts — which finding, which
files, what was done, and what it did to the dependency set or to a resolved classpath — anchored to
commits, so a reader re-checks a row with `git` and `sbt` instead of trusting this table.

**The invariant the ledger makes checkable.** The declared dependency set is the eleven coordinates
`build.sbt` has carried since the build was introduced in `27df2962f`, and no remediation has added,
removed or re-versioned any of them. The only change to a dependency declaration in this build's whole
history is row 9's exclusion, which subtracts a duplicate transitive and adds nothing:

```
coordinates() { grep -E '^[[:space:]]+\(?"(org|io|com)\.' | sed -E 's/^[[:space:]]+//' | sort; }
diff <(git show 27df2962f:build.sbt | coordinates) <(coordinates <build.sbt)
# eleven lines each; the one that differs is discipline-scalatest, parenthesised to carry its exclusion
```

Both sides are compared through process substitution rather than through temporary files: a fixed path
under a shared temporary directory races every other process that follows the same instruction, and
`>` follows a symbolic link already sitting there.

Rows 1 to 5 are the findings raised against the build definition and the CI job while the tree was
being finished, one row each. Row 6 is the gate-runner review, whose twenty-nine findings have a row
each in the second table below. Rows 7 to 12 are the findings of the dependency-safety,
tooling-security and comment reviews of the finished tree. **Impact** is measured rather than assumed
— `none` means no declared coordinate changed and no entry joined or left a resolved Compile or Test
classpath.

| # | Finding | Review checkpoint | Files changed | Resolution | Dependency / classpath impact | Commits |
|---|---|---|---|---|---|---|
| 1 | `config/F01` — LOW, blocking: `Global / excludeLintKeys += logManager` suppressed sbt's unused-key lint, against the no-suppression requirement | sbt Build and Repository Configuration (4 findings, all LOW, 1 blocking) | `build.sbt` | The custom test-logging layer that needed the exclusion was removed, and the exclusion with it; nothing in this build filters or silences a warning | none | added `55fca5ed8`, removed `86d8c8a32` |
| 2 | `config/F02` — LOW: the custom log appender opened a `PrintStream` per logger with no deterministic close | sbt Build and Repository Configuration | `build.sbt` | Removed with the same layer; test reporting is ScalaTest's own `-u` reporter, which owns its files | none | `86d8c8a32` |
| 3 | `config/F03` — LOW: six `sbt.internal.*` types coupled the meta-build to sbt 1.13.0 implementation detail | sbt Build and Repository Configuration | `build.sbt` | Removed with the layer; the build definition now uses public sbt API only | none — the coupling was to the meta-build, never to a module classpath | `86d8c8a32` |
| 4 | `config/F04` — LOW: `IO.createDirectory` mutated the filesystem during setting evaluation | sbt Build and Repository Configuration | `build.sbt` | Removed; the `-u` reporter creates its own report directory when tests run | none | `86d8c8a32` |
| 5 | `ci/F01` — LOW: `>> $BASH_ENV` was unquoted in the sbt installation step | Scala Build CI Infrastructure (1 finding, LOW) | `.circleci/config.yml` | Quoted: `>> "$BASH_ENV"` | none | added `c6e6a0489`, fixed `86d8c8a32` |
| 6 | `gate/F01`–`gate/F29` — 8 HIGH, 16 MEDIUM, 5 LOW, 21 blocking: gate rows that could pass vacuously, unchecked evidence paths, and report and output-path handling | Acceptance Gate Runner / SCRIPTING-INTEGRATION (29 findings) | `scripts/verify-gates.sh` | Rows hardened individually against AAP §0.10.1, which remains the authority for every row and pass condition; each finding has its own row in the second table below, with the state of the script as it now stands | none — the runner declares no dependency and contributes nothing to a classpath. Its dependency-bearing content is the Gate 2 row, which measures the four dependency trees and both exported classpaths | added `d2f5ba43c` (2,705 lines), hardened `86d8c8a32` (+3,743 / −614) |
| 7 | `SECDEP-F02` — HIGH, blocking: the `scala_build21` cache restored `~/.sbt` and `~/.cache/coursier` through branchless prefix fallbacks and saved with `when: always`, so an untrusted branch could persist global sbt plugins or altered dependency bytes into a later trusted build | Dependency Safety and Software Supply Chain | `.circleci/config.yml` | Two fully qualified restore keys, each naming the architecture, one branch, the job and all three build-definition checksums, and never a truncated prefix; `save_cache` runs only for the trunk pipeline (a `when` condition on `<< pipeline.git.branch >>`) and carries no `when` of its own, so the `on_success` default keeps a failed run from saving; the cached path is the Coursier download cache alone. Because CircleCI caches are one project-wide namespace that `restore_cache` searches by prefix, no key is treated as a trust boundary: two steps between the restore and the gate validate what arrived — the sbt home is refused if it carries any `*.sbt`, `*.scala`, credential or `plugins` entry outside `boot/`, and every restored jar and pom is re-checked against the SHA-1 the repository publishes and deleted when it does not match, so an unverified byte is downloaded again rather than executed | none to the declared set or to any resolved classpath; what changes is which cache a build may write and which restored bytes may run | this revision |
| 8 | `SECTOOL-F16` — MEDIUM, blocking: `save_cache` stored the whole `~/.sbt`, the standard home of `credentials.sbt` and `.credentials`, and no scanner examines cache contents | Tooling, Artifact and Secret Security | `.circleci/config.yml` | The cached paths are the two download caches only — `~/.cache/coursier` and `~/.sbt/boot`; the sbt home itself is never cached, and the validation step of row 7 refuses a credential file arriving from anywhere else | none to any classpath | this revision |
| 9 | `SECDEP-F03` — MEDIUM: `discipline-scalatest` 2.3.0 pulled `org.scalatestplus:scalacheck-1-18_2.13:3.2.18.0` alongside the declared `scalacheck-1-20_2.13:3.2.20.0`; different artifact ids, so nothing evicted either, and both publish the same 23 class names with a byte-different `CheckerAsserting` | Dependency Safety and Software Supply Chain | `build.sbt`, `scripts/verify-gates.sh` | The transitive is excluded at the declaration that introduces it, leaving the adapter matching the resolved ScalaCheck as the only one; the Gate 2 row gained a duplicate-class audit that fails on any class name two entries of one exported classpath provide, so the condition cannot silently return | the one classpath change in this ledger: the `strata-basics` Test classpath loses `scalacheck-1-18_2.13:3.2.18.0` — 46 entries to 45, duplicated class names 23 to 0, byte-different duplicates 2 to 0 — while both Compile classpaths and both `strata-collect` classpaths are unchanged, the latter never having carried it | this revision |
| 10 | `SECDEP-F01` — LOW: the reconciled build-configuration and gate-runner reports the dependency review required as evidence were not in the clone, so the remediation decisions could not be verified independently | Dependency Safety and Software Supply Chain | `SCALA_MIGRATION.md` | This section: the ledger of every row above with its files, resolution and measured impact, anchored to commits rather than to a report that lives outside the tree | none | this revision |
| 11 | `CMTBLD-F01`–`F04`, `F87`, `F88` — six LOW, two blocking: migration narration in the `build.sbt` banner and the CI job banner, a paraphrase of the visible compiler options, a duplicated report-path explanation, a comment duplicating README material, and a time-sensitive GPG version claim | COMMENTS — Build, Tooling, CI and Documentation | `build.sbt`, `.circleci/config.yml` | Comment text only: the narration is gone, the surviving comments state the root-project, fork-lifecycle, cache and gate-ownership invariants, and the GPG note is tied to what the block requires (loopback pinentry, GPG 2.1 or later) instead of to what an image happened to ship | none — no setting, coordinate or step semantics changed | this revision |
| 12 | `SECDEP-F04` — LOW: the first version of this section's re-check procedure wrote to the fixed shared paths `/tmp/then` and `/tmp/now`, which race concurrent runs and follow a symbolic link already at that path | Dependency Safety and Software Supply Chain | `SCALA_MIGRATION.md` | The procedure compares both sides through process substitution and writes no file | none | this revision |

### The gate-runner review, finding by finding

Every row below changed `scripts/verify-gates.sh` and nothing else, was introduced with the
runner in `d2f5ba43c` and adjudicated in `86d8c8a32`, and has no dependency or classpath impact:
the runner declares no dependency and contributes nothing to a Compile or Test classpath. The
state column is what this revision's script does, read at the row's own code rather than taken
from the report.

| # | Finding | Verdict | What was wrong | How it stands in this tree |
|---|---|---|---|---|
| 1 | `gate/F01` — MEDIUM, blocking | resolved | Gate 7's manual row deviated from the mandated approval text when the automated half failed | The mandated sentence is emitted unconditionally; no conditional failure text remains in the manual row |
| 2 | `gate/F02` — LOW | resolved | A blank line at end of file made `git diff --check` exit 2 | The file ends with its last statement and one newline; `git diff --check` is clean |
| 3 | `gate/F03` — HIGH, blocking | resolved | The boxing audit selected hot methods by name prefix, so loop helpers such as `scaledInto`, `ternaryFoldFrom`, `buildRows`, `fillRow`, `addInto`, `multiplyInto` and `applyInto` were never disassembled | Selection is the transitive, asserted-closed call graph of the hot methods the specification names, and the row asserts each named method is declared by its class before auditing it |
| 4 | `gate/F04` — MEDIUM, blocking | resolved | Snapshot and restore `rm`, `cp` and `mkdir` were unchecked while `errexit` was disabled inside gate calls, so stale or missing evidence could survive a passing row | Each helper reports through `snapshot_failure` into the framework-error record, and the copy is verified file by file against its source |
| 5 | `gate/F05` — MEDIUM | resolved | Only the JUnit XML was restored, losing Gate 1's text test reports the script promises | A snapshot mirrors every file of the directory it covers rather than a chosen extension |
| 6 | `gate/F06` — HIGH, blocking | resolved | The parity-report parser validated neither field types nor cross-field consistency, so `failed: 0` beside a non-empty `failures` array could pass | The parser rejects booleans and non-integers and enforces the cross-field agreement between the counts and the failure list |
| 7 | `gate/F07` — MEDIUM, blocking | resolved | Gate 4 discarded the exclusion reasons, so a wrong or blank rationale could be published as evidence | The whole `EXCLUDED <fqcn> <reason>` line is compared against the pinned inventory of section (e) |
| 8 | `gate/F08` — MEDIUM, blocking | resolved | The resource grep the specification prescribes was informational while a narrower substitute decided the row | The literal recursive grep for `.ini`, `.csv` and `.properties` over both modules' main sources decides the row, and the semantic scans are supplementary to it |
| 9 | `gate/F09` — HIGH, blocking | resolved | Hidden-class identities were normalised before the deciding difference, so distinct codec-only classes could collapse into one name and escape inspection | The raw difference is the decision set, stated as such in the row's contract; hidden entries are classified rather than merged |
| 10 | `gate/F10` — MEDIUM, blocking | resolved | The warning-policy scan narrowed its scope and ignored hits inside comments rather than enforcing the prescribed recursive grep | The scan covers `build.sbt`, `project/` minus generated output, and both modules' `src` trees, and every hit outside the single allowed option element fails, comments included |
| 11 | `gate/F11` — HIGH, blocking | resolved | The `partial` and `dropped` allow-lists were class-wide, so any method of an allowed class could be excused | Exact `(class, method)` pairs are required, and class-level exceptions remain only for the wholly excluded classes |
| 12 | `gate/F12` — MEDIUM, blocking | resolved | Status grammar, consolidated-target equality, duplicate rows and row cardinality were unenforced | The status grammar is matched exactly, `consolidated:<spec>` must equal the row's own spec, duplicate rows are rejected and the derived 1,876-row cardinality is enforced |
| 13 | `gate/F13` — LOW | resolved | `--help unexpected` exited 0 and silently ignored the unsupported second argument | Arity is decided by `case "$#"`, with a usage error and exit 2 for every arity other than none or one help flag |
| 14 | `gate/F14` — MEDIUM | resolved | Contrary to its own comment, sbt output was not streamed, so a long command printed nothing until it finished | `run_sbt` pipes through `tee` and takes sbt's status from `PIPESTATUS[0]` |
| 15 | `gate/F15` — MEDIUM | resolved | Sourcing the script still read caller arguments, changed directory, truncated artifacts and replaced traps | Every side effect and the trap installation sit in `init_run`; sourcing defines the helpers and the rows and runs nothing, which is how a single row can be exercised |
| 16 | `gate/F16` — MEDIUM, blocking | resolved | Report completion was flagged before the write to the final path succeeded, so an interrupted run could leave a partial report the exit trap refused to retry | The report is written to a temporary file, verified and renamed, and only then is completion recorded |
| 17 | `gate/F17` — LOW, blocking | resolved | A preflight failure produced impossible summary counts through subtraction | Preflight is recorded as its own row kind, nothing is derived by subtraction, and the preflight path reports an incomplete run |
| 18 | `gate/F18` — MEDIUM, blocking | resolved | The Gate 2a Test check required `test-classes` but not the collect main `classes` directory | Both directories are required on the Test internal dependency classpath, which is the proof of the compile-and-test edge |
| 19 | `gate/F19` — MEDIUM, blocking | resolved | Any six level-two headings passed Gate 7; the required sections were never identified | Each `## (a)` to `## (f)` is looked for by its own label, required exactly once and required in order before section (a) is validated |
| 20 | `gate/F20` — MEDIUM, blocking | resolved | The documentation half of Gate 6 was missing, so the demo command needed to appear in no README | The row requires the exact demo command in `README.md` and `strata-basics/README.md` and fails naming the file that omits it |
| 21 | `gate/F21` — HIGH, blocking | resolved | The authoritative collect test-class set was derived from the manifest being audited, so omitting a whole class escaped detection | The class inventories are pinned in the script from AAP sections 0.4.1 and 0.2.2, and the manifest must equal them before any method join |
| 22 | `gate/F22` — MEDIUM | resolved | The source-root parser tokenised on whitespace, so a checkout path containing a space caused a false Gate 2a failure | The row compares the expected source directories rather than splitting printed paths on whitespace |
| 23 | `gate/F23` — MEDIUM, blocking | resolved | The Gate 2a section files were not truncated before parsing, so stale content from an earlier run could satisfy the edge checks | Each section file is started through `safe_truncate`, and the parsed headers are counted and attributed |
| 24 | `gate/F24` — LOW | resolved | Preflight omitted several commands the runner actually uses | The required list carries `cat`, `head`, `tail`, `basename`, `date`, `cp`, `rm`, `mkdir`, `mv` and `tee` beside `git`, `sbt`, `java`, `javap`, `python3` and the text utilities |
| 25 | `gate/F25` — HIGH, blocking | resolved | Overloaded Java test methods collapsed by bare name, reporting 642 methods where the authority is 643 | Methods are keyed by normalised signature, so `DecimalTest`'s two `testValuesOfBigDecimal` methods stay two rows, and the 643-method count is asserted |
| 26 | `gate/F26` — HIGH, blocking | resolved | Components of the ignored `target` path could be symbolic links, redirecting truncation and copies outside the repository | Every output is started through the symlink-refusing, canonicalising write path, and a refused path is a framework error rather than a silent write |
| 27 | `gate/F27` — LOW | recurring | An unused helper and a counter that was maintained but never read | `count_matches` is gone, but the dead-write class this finding named is present again: `GATE_TOTAL`, `GATE7_MEMBER_COUNT` and `GATE7_TABLE_ROWS` are each assigned once and never read. It is open at this boundary under the tooling-security review's own findings on the runner, which belong to the unit that owns the script's rows, and is recorded here rather than closed |
| 28 | `gate/F28` — HIGH, blocking | resolved | `__JVM_LookupDefineClass__` matched the generic identifier pattern and was handed to `javap` as a host class, failing the row spuriously | JVM markers are handled before generic identifiers and reported as non-inspectable instead of being disassembled |
| 29 | `gate/F29` — MEDIUM, blocking | resolved | With `errexit` disabled inside gate calls, many row pipelines were unchecked, so a failing `comm` or redirection could yield a passing row | Framework errors are counted before and after every row, so an unchecked command's failure becomes the row's verdict and is summarised in the report |

**Re-checking a row.** The commands below establish rows 1 to 6 from history and rows 7 to 12 from the
tree as it stands:

```
git log --format='%h %ad %s' --date=short -- build.sbt project .circleci/config.yml scripts
git show 86d8c8a32 -- build.sbt .circleci/config.yml | grep -E 'excludeLintKeys|sbt\.internal|BASH_ENV'
sbt -batch "export strata-basics/Test/fullClasspath"          # no scalacheck-1-18 entry
source scripts/verify-gates.sh; init_run; row_02_dependency_purity
cat target/audit/gate02-duplicate-classes.txt                 # per-classpath duplicate audit
```
