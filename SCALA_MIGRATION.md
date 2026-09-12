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
depending on the port: it is the complete list of behaviours that are deliberately *not* identical to
Java. Every row of it was measured on both sides — the port in this repository and the Java
`2.12.74-SNAPSHOT` jars driven through `jshell` — and the measured values are quoted in the row.

### Delivery state of the port

Both modules are complete in this tree, and this note describes what is in it rather than a plan: a
register that describes absent code as delivered is worse than no register, and one that describes
delivered code as absent is no better.

- **`strata-collect`** — 18 main sources: validation (`ArgCheck`, `Validate`), the `Failure` ADT with
  `FailureReason` and the result aliases, `NamedEnum`, `Named`, `TypedString`, `Decimal` and
  `FixedScaleDecimal`, `DoubleArray`/`DoubleMatrix`/`Matrix`/`DoubleArrayMath`, `Collections`,
  `io.Resources` and `json.Codecs`.
- **`strata-basics`** — 77 main sources: the root contracts (`ReferenceData`, `ReferenceDataId`,
  `Resolvable`, `CalculationTarget`, `StandardId`, `StandardSchemes`), `currency` (16), `date` (20,
  including `DayCount`, the 25 calendar generators, `THBA` and `StandardHolidayCalendars`), `index`
  (18, the sealed `Index` hierarchy with its four data tables, `FloatingRateName` and the five
  observations), `location` (2), `schedule` (6), `value` (7) and `demo/BasicsDemoApp.scala`.
- **Around them** — the six parity baselines, the reference-data manifest, the method-level
  `java-test-mapping.csv`, the capture tooling under `tools/parity-capture/`, the gate runner
  `scripts/verify-gates.sh`, and the tests: 22 test sources in `strata-collect` and 93 in
  `strata-basics`, whose counts the test-scope gate reads from the JUnit XML rather than from here.

No reference data, alias table or lenient pattern of the Java modules is left untranscribed, and no
member of section (a) is left without the replacement its row names. Section (f) gives the commands.

## (a) Symbol table: every `strata-collect` member `strata-basics` uses

The Java `strata-basics` main sources import 29 distinct `strata-collect` targets (22 types plus 7
statically imported `Guavate` members); its test sources add three more (`TestHelper`,
`CollectProjectAssertions`, `Unchecked`). The table below is grouped by those targets and carries one
row per member used, or per group of overloads whose replacement is identical. **72 rows.** "No
target" means the member's job disappeared with the mechanism it served, and the row says what does
the job now.

Every replacement lives in `strata-collect` unless the Module column says otherwise; "collect (test)"
is the module's test scope, visible to `strata-basics` tests through
`dependsOn(strata-collect % "test->test")`.

| # | Java target | Member used | Scala replacement | Module |
|---|-------------|-------------|-------------------|--------|
| 1 | `collect.ArgChecker` | `isTrue` (both overloads) | `ArgCheck.isTrue` (fail-fast) / `Validate.isTrue` (accumulating) | collect |
| 2 | `collect.ArgChecker` | `isFalse` | `ArgCheck.isFalse` / `Validate.isFalse` | collect |
| 3 | `collect.ArgChecker` | `matches` | `ArgCheck.matches` / `Validate.matches` | collect |
| 4 | `collect.ArgChecker` | `notEmpty` (String, array, `Iterable`, `Map`, `Matrix`) | `ArgCheck.notEmpty` / `Validate.notEmpty`, same overload set | collect |
| 5 | `collect.ArgChecker` | `notNaN` | `ArgCheck.notNaN` / `Validate.notNaN` | collect |
| 6 | `collect.ArgChecker` | `notNegative` | `ArgCheck.notNegative` / `Validate.notNegative` | collect |
| 7 | `collect.ArgChecker` | `notNegativeOrZero` | `ArgCheck.notNegativeOrZero` / `Validate.notNegativeOrZero` | collect |
| 8 | `collect.ArgChecker` | `inOrderNotEqual` | `ArgCheck.inOrderNotEqual` / `Validate.inOrderNotEqual` | collect |
| 9 | `collect.ArgChecker` | `inOrderOrEqual` | `ArgCheck.inOrderOrEqual` / `Validate.inOrderOrEqual` | collect |
| 10 | `collect.ArgChecker` | `notNull` | No target — see divergence (c)-3 | — |
| 11 | `collect.ArgChecker` | `noNulls` | No target — see divergence (c)-3 | — |
| 12 | `collect.Decimal` | `of` (`double`, `BigDecimal`, `String`) | `Decimal.of` / `Decimal.ofScaled` / `Decimal.parse`, each returning `Either[Failure, Decimal]` | collect |
| 13 | `collect.Decimal` | `ZERO` | `Decimal.ZERO` | collect |
| 14 | `collect.FixedScaleDecimal` | `of` | `FixedScaleDecimal.of`, returning `EitherNec[Failure, FixedScaleDecimal]` | collect |
| 15 | `collect.Guavate` | `ensureOnlyOne` | `Collections.ensureOnlyOne`, returning `Either[Failure, Option[A]]` | collect |
| 16 | `collect.Guavate` | `toImmutableList` | `scala.collection.immutable.List` (`.toList`) | — |
| 17 | `collect.Guavate` | `toImmutableSet` | `scala.collection.immutable.Set` (`.toSet`) | — |
| 18 | `collect.Guavate` | `toImmutableSortedMap` | `Collections.toSortedMap` (duplicate keys reported as a `Failure`) or `SortedMap` directly | collect |
| 19 | `collect.Guavate` | `toImmutableSortedSet` | `scala.collection.immutable.SortedSet` | — |
| 20 | `collect.Guavate` | `list` | `List(...)` | — |
| 21 | `collect.Guavate` | `stream` | `.iterator` / `LazyList` | — |
| 22 | `collect.Guavate` | `filteringOptional` | `Iterator#collect` / `List#flatMap` over `Option` | — |
| 23 | `collect.Guavate` | `inOptional` | `Option#exists` / `Option#toList` | — |
| 24 | `collect.Guavate` | `tryCatchToOptional` | `scala.util.Try(...).toOption` | — |
| 25 | `collect.Guavate` | `entriesToImmutableMap`, `pairsToImmutableMap` and the rest of the collector set | No target — stdlib builders and `cats.syntax` cover them | — |
| 26 | `collect.MapStream` | `of` and its map/filter/collect chain | `scala.collection.immutable.Map` operations, with `Collections.groupByPreservingOrder` where insertion order matters | collect |
| 27 | `collect.Messages` | `format` | Scala string interpolation (`s"..."`) | — |
| 28 | `collect.array.DoubleArray` | `of` (varargs and `Iterable`) | `DoubleArray.of` — copies its input | collect |
| 29 | `collect.array.DoubleArray` | `filled` | `DoubleArray.filled` | collect |
| 30 | `collect.array.DoubleArray` | `ofUnsafe` | `private[collect] DoubleArray.ofUnsafe`; callers outside collect use `tabulate`, `map`, `mapWithIndex` or `combine` — see divergence (c)-13 | collect |
| 31 | `collect.array.DoubleArray` | `toArrayUnsafe` | `private[collect] DoubleArray.toArrayUnsafe`; public `toArray` copies — see divergence (c)-13 | collect |
| 32 | `collect.array.DoubleArray` | `DoubleArray.class` (Joda meta-bean literal) | No target — there are no meta-beans | — |
| 33 | `collect.array.DoubleMatrix` | `ofUnsafe` | `private[collect] DoubleMatrix.ofUnsafe`; callers use `tabulate`/`ofArrays`/`copyOf` | collect |
| 34 | `collect.array.DoubleMatrix` | `DoubleMatrix.class` (Joda meta-bean literal) | No target — there are no meta-beans | — |
| 35 | `collect.io.CsvFile` | `of` | No target — the index CSV tables are Scala data objects (`IborIndexData`, `OvernightIndexData`, `PriceIndexData`, `FxIndexData`) | — |
| 36 | `collect.io.CsvRow` | `getValue`, `findValue` | No target — a data-object row is a typed Scala value | — |
| 37 | `collect.io.IniFile` | `of` | No target — the INI tables are Scala data objects and the alias/lenient tables live in their families' companions | — |
| 38 | `collect.io.PropertiesFile` | `of` | No target — `country.properties` became `location.CountryData` | — |
| 39 | `collect.io.PropertySet` | `keys`, `value`, `valueList` | No target — same reason as `IniFile` | — |
| 40 | `collect.io.ResourceConfig` | `combinedIniFile` | No target — the `base`/`library`/`application` override chain is deliberately not ported | — |
| 41 | `collect.io.ResourceConfig` | `orderedResources` | No target — same reason | — |
| 42 | `collect.io.ResourceLocator` | `ofClasspath` (+ `getCharSource`) | `io.Resources.readClasspathText(path): IO[String]`, used by fixtures and the demo only | collect |
| 43 | `collect.io.ResourceLocator` | `ofFile` | `io.Resources.readFileText(path): IO[String]` — see divergence (c)-16 | collect |
| 44 | `collect.named.Named` | `getName` | `Named.name` (universal trait, `extends Any`) | collect |
| 45 | `collect.named.ExtendedEnum` | `of` (registry construction) | `NamedEnum.of(values, alternates, lenient, externals)` — an in-code table, no classpath scan | collect |
| 46 | `collect.named.ExtendedEnum` | `lookup`, `find` | `NamedEnum.valueOf(name): Option[A]` (alias-aware) | collect |
| 47 | `collect.named.ExtendedEnum` | `findLenient` | `NamedEnum.parse(name): EitherNec[Failure, A]` (alias, then the ordered lenient rewrites, then alias again) | collect |
| 48 | `collect.named.ExtendedEnum` | `lookupAll`, `lookupAllNormalized` | `NamedEnum.byUpperName`, `NamedEnum.byCanonicalName` | collect |
| 49 | `collect.named.ExtendedEnum` | `externalNames` | `NamedEnum.externalNames(group)`, `externalNamesRaw(group)`, `externalNameGroups` | collect |
| 50 | `collect.named.EnumNames` | `of` (+ `format`, `parse`) | `NamedEnum.of` with `NamedEnum.showByName` / `orderByName` / `hashByName` | collect |
| 51 | `collect.named.NamedEnum` | the marker interface for Java `enum` families | A `NamedEnum[A]` instance in each sealed family's companion | collect |
| 52 | `collect.named.NamedLookup` | `lookupAll` provider contract | No target — there is no provider mechanism to implement | — |
| 53 | `collect.named.CombinedExtendedEnum` | `of` | One `parse`/`tryParse` per union: `Index.parse`, `RateIndex.parse`, `FloatingRateIndex.parse`, `FloatingRate.parse`, each trying its families in the Java order | basics |
| 54 | `collect.tuple.Pair` | `of` (and `getFirst`/`getSecond`) | `Tuple2` (`(a, b)`, `._1`, `._2`) | — |
| 55 | `collect.TypedString` | the validated string base class | `TypedString.TypedStringCompanion[T]` over a `final class ... extends AnyVal with Named` | collect |
| 56 | `collect.result.Result` | `of`, `success`, `failure`, `map`, `flatMap` | `FailureOr[A] = Either[Failure, A]` and `ResultNec[A] = EitherNec[Failure, A]` with cats syntax | collect |
| 57 | `collect.result.Result` | `combine`, `sequence`, `allSuccessful`, `anyFailures`, `countFailures` | `result.combine`, `result.sequence`, `result.flatCombine`, `result.allSuccessful`, `result.anyFailures`, `result.countFailures` | collect |
| 58 | `collect.result.FailureItem` | `of`, `getReason`, `getMessage`, `getAttributes` | The sealed `Failure` ADT — one case class per reason, each with `message` and `attributes: SortedMap[String, String]` | collect |
| 59 | `collect.result.FailureReason` | the ten reasons | The sealed `FailureReason` family with a `NamedEnum` instance | collect |
| 60 | `collect.result.ValueWithFailures` | `of`, `withAdditionalFailures`, `combinedWith` | `ValueWithFailures[A] = Ior[NonEmptyChain[Failure], A]` plus `result.ValueWithFailures.*` helpers | collect |
| 61 | `collect.DoubleArrayMath` | the element-wise array helpers | `DoubleArrayMath`, same operation set, `var`-free | collect |
| 62 | `collect.DoubleArrayMath` | (Guava `DoubleMath.fuzzyEquals`) | `DoubleArrayMath.fuzzyEquals` — scalar and array forms; Guava's three clauses reproduced verbatim, see (c)-9 | collect |
| 63 | `collect.DoubleArrayMath` | (Guava `DoubleMath.isMathematicalInteger`) | `DoubleArrayMath.isMathematicalInteger` | collect |
| 64 | `collect.TestHelper` | `date` | `testkit.TestHelper.date` | collect (test) |
| 65 | `collect.TestHelper` | `list` | `testkit.TestHelper.list` | collect (test) |
| 66 | `collect.TestHelper` | `caputureLog` (Java spelling) | `testkit.TestHelper.captureLog` (and `captureStdOut`, `snapshot`) | collect (test) |
| 67 | `collect.TestHelper` | `coverImmutableBean`, `coverBeanEquals`, `coverEnum`, `coverPrivateConstructor` | No target — reflective bean sweeps are replaced by ScalaCheck equality, `Show` and round-trip properties | — |
| 68 | `collect.TestHelper` | `assertSerialization` | No target — replaced by the circe round-trip properties of section (e) | — |
| 69 | `collect.TestHelper` | `assertJodaConvert` | No target — Joda-Convert is gone; the string form is asserted through `Show` and the string codecs | — |
| 70 | `collect.TestHelper` | `assertUtilityClass` | No target — a Scala `object` has no constructor to hide | — |
| 71 | `collect.CollectProjectAssertions` | `assertThat(Result)` / `assertThat(ValueWithFailures)` | `testkit.ResultMatchers` — `haveValue`, `beFailureWith`, `value`, `failures`, `haveFailureMessageMatching` | collect (test) |
| 72 | `collect.Unchecked` | `wrap` | `scala.util.Try` / `scala.util.Using`, or a plain expression where nothing was checked | — |

Two Guava members that `strata-basics` used directly, rather than through `strata-collect`, are
replaced in the same place and are rows 62 and 63 above: `DoubleMath.fuzzyEquals` (the fixed-rate
`convertedTo` overloads) and `DoubleMath.isMathematicalInteger` (`CurrencyAmount.toString`,
`FxRate.toString`). Guava's `Immutable*` collections become `scala.collection.immutable`, its
`Splitter`/`Joiner` become `String#split`/`mkString`, and `Suppliers.memoize` becomes a `lazy val`.
Joda-Beans and Joda-Convert have no replacement member: the bean machinery becomes ordinary Scala
values with circe codecs, and `@ToString`/`@FromString` become `Show` and `NamedEnum.parse`.

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
| `collect.io.*` except a text reader | The INI/CSV/properties readers served the runtime configuration mechanism this port replaces with Scala data. Only the classpath/file text reader survives, as `io.Resources`, for fixtures and the demo |
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
| The `[types]` section of `Index.ini`, `RateIndex.ini` and `FloatingRateIndex.ini` | Each declared one `CombinedExtendedEnum` union over other families. Each becomes a `parse`/`tryParse` in the corresponding Scala companion, trying the same families in the same order (section (a), row 53) |
| `HolidayCalendarIniLookup`, `IborIndexCsvLookup`, `OvernightIndexCsvLookup`, `PriceIndexCsvLookup`, `FxIndexCsvLookup`, `FloatingRateNameIniLookup`, `CurrencyDataLoader`, `GlobalHolidayCalendarLookup` (in `strata-basics`) | Their whole responsibility is met by the `*Data.scala` objects and `NamedEnum`; there is nothing left to load |
| `ImmutableHolidayCalendarDeserializer`, `META-INF/org/joda/beans/JodaBeans.ini`, `GlobalHolidayCalendars.bin` | Joda-Beans serialization and the generated calendar cache are not carried — see divergence (c)-31 |
| `java.io.Serializable` / `Externalizable` support on any type | Replaced entirely by circe codecs |
| A Java-callable façade | Excluded by design decision D-3 — see divergence (c)-4 |
| Java test artefacts exercising removed machinery: `HolidayCalendarIniLookupTest`, `FailureExceptionTest`, `FailureItemExceptionTest`, `IllegalArgFailureExceptionTest`, `ParseFailureExceptionTest`, the `HolidayCalendarData*.ini` test fixtures, `ImmutableHolidayCalendar-Old.json` | Their subjects are gone. The method-level record of every dropped, consolidated and ported Java test is `strata-basics/src/test/resources/manifest/java-test-mapping.csv` |

Of the `Guavate` and `MapStream` surfaces, only the members listed in section (a) are ported;
`Collections.scala` holds `ensureOnlyOne`, `toSortedMap`, `groupByPreservingOrder` and the
`NonEmptyChain` helpers, and nothing else.

## (c) Deliberate divergences from the Java original

Each row is a behaviour that is **not** identical to Java `2.12.74-SNAPSHOT`, deliberately. Measured
values quoted below were taken from this repository's build and from the Java jars driven through
`jshell`. Every row describes code that is in the tree; row numbers are referenced from sections (a),
(d), (e) and (f), so they are stable and are never reused.

| # | Area | Java behaviour | Port behaviour | Why |
|---|------|----------------|----------------|-----|
| 1 | Error model | A data failure throws — `IllegalArgumentException`, `IllegalStateException`, `ScheduleException`, `ReferenceDataNotFoundException` | The same failure is returned: `Either[Failure, A]`, or `EitherNec[Failure, A]` where several causes accumulate, over a sealed `Failure` ADT with ten reasons. `ScheduleException` becomes `Failure.Invalid` carrying a `definition` attribute; FX failures become `Failure.CurrencyConversion`; a missing calendar becomes `Failure.MissingData` | Explicit, total error handling (Rule 5). A failure that depends on argument *data* belongs in the signature |
| 2 | Retained throws | Every precondition throws | A caller-contract or numeric-edge precondition still throws `IllegalArgumentException`, through `ArgCheck`, and is documented on the member: array and matrix index/dimension errors; a calendar query outside years 0–9999; a NaN produced by arithmetic on infinite `CurrencyAmount` operands; `Decimal` overflow past 18 digits; `DayCount.yearFraction` with dates out of order or without the schedule information the day count reads | A contract violation is a programming error, not a data outcome; putting it in the signature would tax every correct caller |
| 3 | Null arguments | Rejected with `IllegalArgumentException` from `ArgChecker.notNull` (measured: `StandardId.of(null, "v")` → `Argument 'scheme' must not be null`) | **Outside the contract of every public entry point.** `null` is not a value the port accepts, guards or documents; passing one raises `NullPointerException` where the argument is dereferenced, except `Currency.of(null)`, which happens to answer `Left(Failure.Parsing("Currency name not found: null"))` because it is a table lookup. See (c)-3 below | The `ArgChecker.notNull` family is deliberately not ported, and Scala code has `Option` for absence. Guarding `null` in Scala signatures would pay for a Java hazard the port has no Java callers to protect |
| 4 | Public API shape | Java types: `Optional`, `java.util` collections, Guava `Immutable*`, checked names | Scala-native throughout: `Option`, `scala.collection.immutable`, `cats.data.{NonEmptyList, NonEmptyChain, Validated, Ior, Kleisli}`, cats typeclass instances. No Java-callable façade and no interop shim | Design decision D-3. A façade would constrain every signature to what Java can express |
| 5 | Named families | `ExtendedEnum` reads INI files off the classpath at class-initialisation time; applications extend a family by adding a provider or an INI override | Each family is a **closed** sealed type whose instances exist only in its companion, with a `NamedEnum[A]` instance. Runtime extensibility is gone. The *behaviour* the INI files encoded is kept as Scala data: alias tables, `[externals.FpML]`/`[externals.SWIFT]` groups and the ordered `[lenientPatterns]` rewrites, so `parse` resolves exactly the names Java resolved | Rule 4, and a closed family is exhaustively checkable by the compiler. `main` sources reference no `.ini`, `.csv` or `.properties` resource |
| 6 | `Currency` | Any three upper-case letters mint a currency, guessing zero minor units and USD triangulation (measured: `Currency.of("XYZ")` → `XYZ`, `minorUnitDigits=0`, triangulation `USD`) | The closed set of the 74 configured currencies. An unknown code is `Left(Failure.Parsing("Currency name not found: XYZ"))` (measured). `Country`, by contrast, keeps Java's open code space — any `[A-Z][A-Z]` is accepted (measured: `Country.of("ZZ")` → `Right(ZZ)`) | Rule 4 closes the currency family; AAP Conflict 4. A minted currency with guessed conventions is a silent data error, and `Country` is not a named-enum family in the first place |
| 7 | `FxIndex` | `FxIndex.of(pair)`/`of(name)` mint an index for an unconfigured pair through `createFxIndex`, using the pair's default calendar and a two-day maturity offset (measured: `FxIndex.of("GBP/SEK")` → an index) | The closed set of the 16 configured rows. `of(pair)` answers the configured index with the lowest name, as Java's `min` does, and `Left(Failure.Parsing)` for an unconfigured pair. `createFxIndex` is not ported | Rule 4; AAP Conflict 7 |
| 8 | `ReferenceDataNotFoundException` | Thrown by `ReferenceData.getValue` and by `resolve` | Not ported. `getValue(id)` is `Either[Failure, T]` with `Failure.MissingData` carrying the id as an attribute; `findValue(id)` is `Option[T]`; `containsValue(id)` is unchanged | Row 1, applied to reference data. The exception type had no other use |
| 9 | Tolerance comparison at NaN | Two NaNs **are** fuzzy-equal — Guava's third clause `(isNaN(a) && isNaN(b))` (measured: `DoubleMath.fuzzyEquals(NaN, NaN, 0.1)`, `DoubleArrayMath.fuzzyEquals([NaN],[NaN],1e-9)` and `DoubleArray.of(NaN).equalWithTolerance(…)` all `true`) | **No divergence — the authority is reproduced.** The private comparison every public entry point delegates to is Guava's algorithm verbatim, `copySign(a - b, 1.0) <= tolerance \|\| a == b \|\| (isNaN(a) && isNaN(b))`, so the same three expressions are `true` in the port too (measured). Two consequences follow from the clauses rather than from a decision: an **infinite tolerance equalises any two values**, the two infinities included (`fuzzyEquals(+Inf, -Inf, +Inf)` and `fuzzyEquals(0.0, +Inf, +Inf)` are `true`, while `fuzzyEquals(+Inf, -Inf, MAX_VALUE)` is `false`), and the zero-comparing variants are unaffected because the third clause cannot fire against zero (`fuzzyEqualsZero([NaN], 1e-9)` is `false` and `fuzzyEqualsZero([+Inf], +Inf)` is `true`, both sides alike). See (c)-9 below | AAP §0.3.3's operative requirement is to reproduce Guava's semantics, and §0.4.2 states it the same way; the parenthetical gloss "NaN never fuzzy-equal" misdescribes Guava's own implementation, so the operative requirement governs and the gloss is what needs correcting |
| 10 | Tolerance argument | Guava rejects a NaN tolerance (`tolerance (NaN) must be >= 0`) | Rejected too, through `ArgCheck.notNaN`, with the port's message `Argument 'tolerance' must not be NaN` (measured). The private near-zero test in `ArgCheck`/`Validate` and `DoubleArrayMath`'s zero comparison are the same function of value and tolerance on every input — `abs(x) <= tolerance \|\| x == 0.0`, the both-NaN clause being unable to fire against zero — so an infinity counts as near zero at an *infinite* tolerance on both (measured: `ArgCheck.notZero(+Inf, +Inf, "x")` throws `Argument 'x' must not be zero` and `fuzzyEqualsZero([+Inf], +Inf)` is `true`) and a NaN counts as near zero on neither | The message set is the port's own; the two tests are deliberately kept in separate files rather than one calling the other, because that object checks its own tolerance through this one and calling back would tie the two into a cycle |
| 11 | Numeric precondition types | `DoubleArray.EMPTY.min`/`max` → `IllegalStateException`; `subArray(4)` on a 3-element array → `IndexOutOfBoundsException`; `DoubleMatrix.identity(-1)` → `NegativeArraySizeException`; `DoubleMatrix.filled(0, -1)` → the empty matrix (all measured) | `IllegalArgumentException` through `ArgCheck`, with Java's message text preserved where Java had one (`Unable to find minimum of an empty array`, `Array index out of bounds: 4 > 3`) and the argument named where it did not (`Argument 'size' must not be negative but has value -1`); `filled(0, -1)` and `of(0, -1)` now fail rather than returning the empty matrix. Index access is unchanged — `get(-1)` raises `ArrayIndexOutOfBoundsException` on both sides. A negative length rejects the call rather than producing a value, with one exception type and one message shape across all four array and matrix size paths: `DoubleArray.filled(-1)`, `DoubleArray.filled(-1, 2.0)` and `DoubleArray.tabulate(-1)` check through `ArgCheck.notNegative` before allocating and report `Argument 'size' must not be negative but has value -1`, matching `DoubleMatrix.identity(-1)` exactly (all measured), and `DoubleMatrix.filled(0, -1)` names its own argument (`Argument 'columns' …`); no port path lets the allocator's `NegativeArraySizeException` escape | AAP §0.3.3 classes array and matrix dimension errors as fail-fast `ArgCheck` invariants, and checking before allocating is what stops a negative or huge dimension reaching the allocator. **A caller must not discriminate on the exception type of a precondition failure across this boundary** |
| 12 | Ragged matrix input | `DoubleMatrix.copyOf` accepts a ragged `double[][]` and returns a value whose `total()` and `get(row, col)` then throw `ArrayIndexOutOfBoundsException`, while `toString` renders each row at its own length (all measured) | A matrix is rectangular by contract: `ofArrays` rejects a wrong-length row (`Function returned array of incorrect length 1, expected 2`) and the JSON decoder refuses a ragged payload (`Expected every row of the matrix to hold the same number of elements`). Ragged input has no defined matrix meaning, and Java's permissiveness here — which produced a value whose own reads throw — is not preserved as a feature. `copyOf` rejects it as well, before anything is cloned — `copyOf([[1,2],[1]])` reports `Array cannot be copied as row 1 is of length 1, expected 2` and `copyOf([[1],[1,2]])` names length 2 against 1 (both measured) — where Java copied the array and handed back a value whose own `total`/`get` then threw. Rendering is total on this side whatever a value was built from: `toString` renders each row at its own length, as Java's did | A `[T]` total type with copy-safe factories should not be able to produce a value whose own reads fail, and the three factories plus the JSON decoder now answer raggedness the same way |
| 13 | Array aliasing | `DoubleArray.ofUnsafe`/`toArrayUnsafe` and `DoubleMatrix.ofUnsafe` are public, and alias the caller's array | They are `private[collect]`. The whole public surface copies: `of`, `copyOf`, `filled`, `tabulate`, `toArray`, and the matrix equivalents. `strata-basics` builds arrays with `tabulate`, `map`, `mapWithIndex` and `combine` instead | Rule 3 immutability, enforced by the compiler rather than by a documentation convention |
| 14 | `DoubleArrayMath.sortPairs` | An in-place recursive dual-array quicksort (`dualArrayQuickSort`) that **mutates the caller's arrays**, is not stable, and is `O(n²)` in the worst case | Pure: it returns fresh arrays and leaves its arguments untouched, sorting a stable bottom-up merge sort over one `Array[Int]` permutation plus one `Array[Int]` buffer that the passes alternate between rather than copying back over, comparing `java.lang.Double.compare` directly — stable, no boxing, `O(n log n)` worst case, two allocations regardless of length, one for keys already in ascending order (an `O(n)` scan answers with the identity permutation, which is what a stable merge of such an input produces), and the same length-mismatch message (`Arrays cannot be sorted as they differ in length`). The measured cost is about 1.84× the Java constant on random keys at n = 10⁵–10⁶, at roughly 40 bytes per element; the complexity class is unchanged | Immutability (Rule 3) rules out sorting the caller's arrays, stability is what lets a value of any element type follow its key, and the permutation sort is what keeps the operation `var`-free and boxing-free. The constant factor is the accepted price of those properties |
| 15 | Primitive callbacks | Java uses its own `collect.function.*` interfaces (`DoubleTernaryOperator`, `IntIntDoubleConsumer`, …) | Those interfaces are not ported (section (b)). Where a primitive callback is needed to keep a hot path free of boxing, the type is a single-abstract-method trait declared beside its user: `DoubleArray.DoubleTernaryOperator` (taken by `combineReduce` instead of a three-argument function) and `DoubleMatrix`'s `ElementAction`, `ElementFunction`, `RowArrayFunction`, `RowArrayObjectFunction`. Call sites stay ordinary Scala lambdas | `Function3` and friends are specialised over nothing, so the standard function types cannot satisfy the no-boxing requirement for these methods |
| 16 | Text loading | `ResourceLocator` decodes leniently, substituting a replacement character for malformed input, and reads a resource of any size | `io.Resources` decodes UTF-8 **strictly** — malformed or unmappable input fails the effect with an `IOException` naming the source — and refuses a source larger than the public `Resources.MaxBytes` ceiling (64 MiB). `readFileText` is an unconfined filesystem reader, exactly as the Java original was: it is a fixture and demo loader, and **callers must not pass it an untrusted path** | A substituted character inside a captured baseline is a silently altered expectation: the measurement still runs, against a value nobody captured |
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
| 29 | JSON detail | Joda-Beans wire forms | The port's own shapes, described in section (e). Three deliberate narrowings: the `LocalTime` encoder is the port's own so that a time renders `11:00` rather than `11:00:00`; the tagged-double decoder refuses a JSON *number* that is not finite, since the tags are the only spelling the encoder produces for those values; and the `Rounding` decoder rejects an unknown field rather than ignoring it | Each accepts strictly less than a lenient reading would, and only payloads the port could never have written |
| 30 | Serialization compatibility | Joda-Beans JSON and binary, plus `java.io.Serializable`/`Externalizable` | Neither is supported or tested, for any type. `ImmutableHolidayCalendar-Old.json` is not readable, `assertJodaSerialization` has no counterpart, and the `ImmutableHolidayCalendar` JSON shape is the port's own | Reflective serialization is what design decision D-5 and Rule 6 remove |
| 31 | Holiday calendar cache | `GlobalHolidayCalendars.bin`, a generated cache read at class-initialisation time, and `main` method that writes it | Not carried. The 25 generators are pure Scala functions, memoised as `lazy val`s, so a calendar is generated once, on first use | A binary cache in the classpath is the runtime-loading mechanism this port removes |
| 32 | Test-layout and manifest details | — | Two test sources sit outside the AAP §0.3.1 enumeration: `strata-collect/src/test/scala/.../testkit/ResultMatchersSpec.scala`, and `ArbitrariesSpec` declared inside `Arbitraries.scala`. `java-test-mapping.csv` carries one method-level `dropped` row beyond the five class-level exclusions (`ImmutableHolidayCalendarTest.test_readOldJodaFormat`, dropped with Joda wire compatibility), quotes `scala_test_name` per RFC 4180 on the 80 rows whose test name contains a comma, and qualifies two overloaded Java methods with an erased parameter list | Each was required to make a contract testable or a manifest unambiguous. The gate script must parse the manifest with a comma-tolerant CSV reader and allow the one method-level exclusion |
| 33 | Failure-message rendering of caller-supplied text | The text handed to a parse or a check is interpolated into the message as it stands, unbounded and unescaped, so the message is as large as the input and a line break in the input puts one in the message (measured against the `2.12.74-SNAPSHOT` jars, message length for a ten-thousand-character input: `BusinessDayConvention.of` 10,038, `Currency.of` 10,073, `CurrencyPair.parse` 10,023, `CurrencyAmount.parse` 10,040, `FxRate.parse` 10,014, `StandardId.parse` 10,027, `StandardId.of` with a rejected scheme 10,072, `ArgChecker.matches` 10,058; and `BusinessDayConvention.of("EUR\nUSD")` reports `BusinessDayConvention name not found: EUR`, a line feed, then `USD`). Java's `Decimal` is the one exception: its scanner threw a `NumberFormatException` naming the offending character rather than the text (measured), so the text this port quotes there is the port's own wording | Every path that returns a failure quoting caller-supplied text renders that text through `Failure.describeInput`, whose bound is `Failure.MaxDescribedInput` = 64 characters plus a three-character marker: `NamedEnum.parse` and therefore every closed family, `Currency.of` and `parse`, `CurrencyPair.parse`, `CurrencyAmount.parse`, `FxRate.parse`, `StandardId.of` and `parse`, `StandardSchemes.splitTicMic`, `Decimal.of` and `parse`, `Validate.matches` in both its forms, `Tenor.parse`, `Frequency.parse` and `FloatingRate.parse`. Two consequences are measured on this build. A ten-thousand-character input renders to **at most 67 characters**, so a message is its fixed wording plus 67 whatever arrives - `Currency.of` with that input reports 25 + 67 characters where Java reported 10,073. And a line break cannot reach a message: a line feed renders as the two characters `\n`, a carriage return as `\r`, a tab as `\t`, and every other ISO control character together with U+2028 and U+2029 as a six-character `\uXXXX` escape. Text within the bound and free of control characters is quoted **byte-identically**, so every ordinary message - every rejected currency code, pair, amount, rate, identifier, numeral and argument of a realistic size - reads exactly as it did before, which is why no expectation of the committed suites changed except the two that were written over a control character or beyond the bound. One asymmetry remains deliberately: `ArgCheck` precondition failures are **thrown** rather than returned, are outside this contract, and still echo their argument as it stands, as Java's `ArgChecker` did (measured: `ArgCheck.matches` with a ten-thousand-character argument throws a 10,058-character message, the same length Java's `ArgChecker.matches` produced) | A failure message is read where reading is line-oriented and of finite size - a log, a report, a line of a console - so text that reached the library from outside it must not be able to forge a line of that log (CWE-117) or to make the message as large as the input. Rendering at the point of interpolation is what leaves the wording of every ordinary failure unchanged, so the property is gained without restating what any message says |

### (c)-3 — Null arguments are outside the contract

The Java module guarded `null` at nearly every public entry point with `ArgChecker.notNull`, and the
port deliberately does not port that family (section (a), rows 10 and 11): in Scala, absence is
`Option`, and a `null` reaching one of these methods is a defect in the calling code rather than a
data condition the library should describe. The consequence is worth stating plainly, because it is
visible and it is not uniform.

Measured on this build, with the Java value from the same expression against the `2.12.74-SNAPSHOT`
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

So: **no public entry point of either module accepts `null`, and none promises a particular failure
for it.** `Currency.of` answering `Left` is an artefact of its implementation — it is a lookup in a
table of 74 codes, and a missing key is a missing key — not a guarantee, and no other entry point
matches it. Code that has a possibly-absent string should hold it as `Option[String]` and decide what
absence means before calling; `Failure` describes *data* that is wrong, and a missing reference is not
data.

Nothing about this is a parity regression: Java rejected `null` too, just with a different exception
type. The `Either`-returning surface is for values a caller can legitimately supply.

### (c)-9 — Tolerance comparison and NaN

The AAP is internally inconsistent here, and the resolution is recorded so that it reads as a decision
rather than an accident. §0.3.3 directs `DoubleArrayMath` to "reproduce Guava's semantics" and then
fixes those semantics parenthetically as "`NaN` (never fuzzy-equal, not an integer)", while §0.4.2
states the requirement as "Guava semantics for `NaN`/infinities preserved". The parenthetical is
factually wrong about the library it describes: Guava's algorithm is
`copySign(a - b, 1.0) <= tolerance || a == b || (isNaN(a) && isNaN(b))`, whose third clause makes two
NaNs equal (measured: `true`).

The port implements the **operative** requirement — reproduce Guava — so the private comparison that
all five public entry points delegate to is that algorithm verbatim, on the `java.lang.Math.copySign`
intrinsic. The parenthetical gloss is the casualty of the inconsistency: it describes neither Guava
nor the port, and correcting it in the AAP is the follow-up this row records. The consequences, all
measured against the built classes of this tree:

- `DoubleArrayMath.fuzzyEquals(NaN, NaN, 0.1)` is `true`, as in Java/Guava, and so is it at a zero
  and at a maximal tolerance; `fuzzyEquals(NaN, 1.0, 1e-9)` is `false`.
- The array form and `DoubleArray.equalWithTolerance` inherit it: `[NaN]` vs `[NaN]` at 1e-9 is
  `true` on both sides, as is `[NaN, 1.0]` vs `[NaN, 1.0]`.
- A tolerance comparison is consequently never *stricter* than exact equality, and reflexivity is
  total — whatever bit-for-bit `==` says of two values, `fuzzyEquals` agrees.
- An **infinite tolerance equalises any two values**, the two infinities included, because an
  infinite magnitude does not exceed an infinite tolerance: `fuzzyEquals(+Inf, -Inf, +Inf)` and
  `fuzzyEquals(0.0, +Inf, +Inf)` are `true`, while at a finite tolerance
  `fuzzyEquals(+Inf, -Inf, MAX_VALUE)` is `false`. Each infinity is fuzzy-equal to itself at any
  tolerance.
- The zero-comparing variants cannot be affected, since Guava's third clause cannot fire against
  `0.0`: `fuzzyEqualsZero([NaN], 1e-9)` and `DoubleArray.of(NaN).equalZeroWithTolerance(1e-9)` are
  `false`, and `fuzzyEqualsZero([+Inf], +Inf)` is `true` — the same value `ArgCheck`'s private
  near-zero test answers for that input, so the two agree on every input (row 10).
- A NaN tolerance is rejected on both sides; only the message differs (row 10).

Nothing in the port is therefore stricter than Java at NaN, and a caller holding two NaN-bearing
arrays needs no explicit comparison of its own. Numerical parity is unaffected either way — the
captured baselines carry no fuzzy expectation, and the parity harness uses its own delta (row 19).

### (c)-20 — A composite calendar id inside a linked id

`HolidayCalendarId.of("EUTA+GBLO~USNY")` names a calendar linked from two parts, the first of which is
itself composite. Resolution in both implementations tries the **whole** name first, so reference data
that holds a pre-combined calendar under this exact id answers with it; the two implementations differ
only in what happens next.

Java splits on the outer separator and performs a *raw* store lookup per part —
`refData.queryValueOrNull(splitId)`, which for `ImmutableReferenceData` is a plain map read — so the
part `EUTA+GBLO`, which standard reference data does not hold as a whole, is absent and resolution
fails:

```
Java:  HolidayCalendarId.of("EUTA+GBLO~USNY").resolve(ReferenceData.standard())
       -> ReferenceDataNotFoundException: Reference data not found for 'EUTA+GBLO'
          of type 'HolidayCalendarId' when finding 'EUTA+GBLO~USNY'
Port:  HolidayCalendarId.of("EUTA+GBLO~USNY").resolve(ReferenceData.standard)
       -> Right(linked calendar, id and name "EUTA+GBLO~USNY")
```

The port resolves a composite part by asking it, which tries its own whole name and then its own
parts, and only then links the results. Three things follow, all measured:

- The divergence is **strictly more permissive**. It can only turn a Java failure into a success:
  every id Java resolves, the port resolves to the same calendar, and the port never fails where Java
  succeeds. `GBLO+USNY`, `EUTA+GBLO` and `GBLO~USNY` resolve identically on both sides.
- Java is inconsistent with itself here, which is why this reading was taken as the intended one:
  `ReferenceData.standard().findValue(HolidayCalendarId.of("EUTA+GBLO"))` answers with a calendar in
  Java (`isPresent` is `true`), because
  that path goes through the id's own resolver — the same id fails only when it appears as a *part*
  and is looked up raw.
- A genuinely missing calendar still fails, all-or-nothing, and the two implementations report it at
  different depths: for `XXXX+GBLO~USNY` the port reports the innermost missing simple calendar
  (`Failure.MissingData`, attributes `id -> XXXX`, `compositeId -> GBLO+XXXX`) while Java names the
  composite part (`Reference data not found for 'GBLO+XXXX' … when finding 'GBLO+XXXX~USNY'`). The
  message text and attribute names are otherwise the Java ones.

Anyone needing byte-exact Java parity, including its failure, should resolve a composite part with a
single `findValue` on that part instead of recursing — a one-line change in
`HolidayCalendarId.resolvePart` — and should expect `EUTA+GBLO~USNY`-shaped ids to stop resolving.

## (d) Covered holiday calendar set

The Java module generated its standard calendars from rules and cached them in
`GlobalHolidayCalendars.bin`; the port keeps the rules as pure Scala functions, memoised as
`lazy val`s, and carries no cache (divergence (c)-31). The built-in set is the same 30 values the Java
registry published, and `ReferenceData.standard` is exactly that set keyed by `HolidayCalendarId`.

**Generated from rules, 1950–2099 (24 calendars):** `AUSY` `BRBD` `CAMO` `CATO` `CHZU` `CZPR` `DEFR`
`DKCO` `FRPA` `GBLO` `HUBU` `JPTO` `MXMC` `NOOS` `NYFD` `NYSE` `NZAU` `NZBD` `NZWE` `PLWA` `SEST`
`USGS` `USNY` `ZAJO`.

**Generated with its own range:** `EUTA`, over 1997–2099, adding holidays only from 2000 onwards.

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
codecs where a shape needs one — with no reflection anywhere on the codec path. The rules below are
implemented once, in `strata-collect/src/main/scala/com/opengamma/strata/collect/json/Codecs.scala`.

**General rules**

- **Named types are bare strings.** Every `Named` family encodes as its canonical name and decodes
  through `NamedEnum.parse`, so the JSON of a day count, convention, index, currency or calendar
  identifier is the same string Joda-Convert produced: `"GBP"`, `"Following"`, `"GBLO+USNY"`, `"P3M"`,
  `"3M"`.
- **Open string-typed values** (`CurrencyPair`, `Country`, `HolidayCalendarId`, `Tenor`,
  `MarketTenor`, `Frequency`, `StandardId`, `Decimal`, `FixedScaleDecimal`) use the same
  string-in/string-out shape through `Codecs.parsedStringCodec`, printing what `toString` prints.
- **Products use Java property names as keys**, in declaration order, and `None` fields are omitted:
  every product encoder is wrapped in `Codecs.dropNulls`.
- **Every `Double` is tagged.** A finite value is a JSON number; `NaN`, `Infinity` and `-Infinity` are
  the strings `"NaN"`, `"Infinity"`, `"-Infinity"`. The decoder accepts a number or one of those three
  strings, and refuses a number that is not finite (divergence (c)-29). Whether a non-finite value is
  *valid* is the type's decision, not the codec's — `CurrencyAmount` rejects `NaN` and accepts the
  infinities, as Java did.
- **Sealed families use circe's own wrapper object**, keyed by the constructor name —
  `{"HalfUp":{…}}`, `{"MissingData":{…}}` — rather than a discriminator field, so no
  `circe-generic-extras` dependency is needed and none is declared: `build.sbt` asks for
  `circe-core`, `circe-generic` and `circe-parser` and nothing else.
- **Validated types decode through their smart constructor**, `Codecs.validatedDecoder` feeding a
  derived decoder for the constructor fields into the type's own `of`, so a payload that violates an
  invariant fails with a `DecodingFailure` carrying the joined failure messages rather than producing
  an invalid value. Their encoders stay derived, because an in-memory value is already valid.
- **Byte stability.** Map-like fields are `SortedMap`/`SortedSet` (`Failure.attributes`,
  multi-currency amounts by currency code, holiday sets by date) or carry their order as part of the
  value (`FxMatrix` currencies), and product fields encode in declaration order, so two equal values
  built differently encode to identical bytes.
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
| `Currency`, `BusinessDayConvention`, `RollConvention` (all 45 values, `Day15` and `DayMon` included), `PeriodAdditionConvention`, `DateSequence`, `StubConvention`, `FloatingRateType`, `ValueAdjustmentType`, `FailureReason`, `IborIndex`, `OvernightIndex`, `PriceIndex`, `FxIndex`, `FloatingRateName` | Bare canonical-name string, through `Codecs.namedEnumCodec` — the same string Joda-Convert wrote |
| `CurrencyPair`, `Country`, `HolidayCalendarId`, `Tenor`, `MarketTenor`, `Frequency`, `StandardId`, `Decimal`, `FixedScaleDecimal` | Bare string in the Java `toString` form (`"EUR/USD"`, `"GB"`, `"GBLO+USNY"`, `"3M"`, `"ON"`, `"P3M"`, `"scheme~value"`), through `Codecs.parsedStringCodec` |
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

Always pass `-batch`: a bare `sbt` shell does not return. On a small host, set
`SBT_OPTS="-Xmx1200m -Xss8m -XX:MaxMetaspaceSize=512m"` first.

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
`target/test-reports/*.xml` and the codec-coverage list, and exits non-zero if any gate fails. The
gates it runs are:

| Gate | What it establishes |
|---|---|
| 1 | Both modules compile and every spec passes |
| 2 | Dependency purity: no Guava, no Joda, no Java `strata-collect` jar on either module's compile or test classpath |
| 2a | Exactly two Scala-only sbt projects, zero `.java` files, and the `strata-basics → strata-collect` edge |
| 3 | Numerical parity: every parity spec green with zero failed rows, at 1e-9 absolute and relative |
| 4 | Serialization: every codec-bearing type round-trips, and encoding is byte-stable |
| 5 | No `var` in either module's main sources; no boxing in the numeric hot paths; no `null` and no `throw new` outside `ArgCheck`; `IO` only at the edges; the typeclass instance inventory; closed families and reference-data fidelity |
| 6 | No reflection on the codec path, proved by a class-load delta between an encode-and-decode run and a baseline run |
| 7 | This note: present, its six sections present, and its symbol table at least as large as the set of collect members `modules/basics/src` references |
| 8 | Class-file major version 65 |
| 9 | Warning-clean: `-Werror` in the common settings and no `@nowarn`, `@SuppressWarnings` or `-Wconf` anywhere |
| 10 | No `java.util` collection, `Optional`, stream or function type in either module's public API, in source or in bytecode |
| — | Test scope at least equal to the Java suites, joined method by method through `java-test-mapping.csv`, and the Maven tree unchanged |

**Gate 7 is this note, and it has two halves.** The automated half is the row above: the script
checks that `SCALA_MIGRATION.md` exists, that its six sections are present as `## ` headings, and
that section (a) carries at least as many table rows as there are distinct `strata-collect` members
referenced from `modules/basics/src` — a count the script computes with the specification's own grep
rather than hardcoding. Measured on this revision: **51** referenced members against the **72** rows
of section (a), and six headings. The manual half is an approving pull-request review by a
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
`awk -F,`, because 80 rows quote a test name containing a comma, and it allows the one method-level
`dropped` row alongside the five class-level exclusions. Forked tests write their parity reports to
`target/parity-report` and their JUnit XML to `target/test-reports`, both under the repository root,
whichever project ran them, because `build.sbt` hands both projects those two absolute paths.
