Strata
======

[![Strata Build Status](https://circleci.com/gh/OpenGamma/Strata.svg?style=shield)](https://strata.opengamma.io) [![License](http://img.shields.io/:license-apache-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0.html)

This repository contains the source code of [Strata](https://strata.opengamma.io),
the open source analytics and market risk library from [OpenGamma](https://opengamma.com/).

Strata is released as Open Source Software under the
[Apache v2.0 license](https://www.apache.org/licenses/LICENSE-2.0.html). 

[![OpenGamma](https://s3-eu-west-1.amazonaws.com/og-public-downloads/og-logo-alpha.png "OpenGamma")](https://opengamma.com/)


Using Strata
------------

Documentation for Strata can be found at the [Strata home page](https://strata.opengamma.io).

To use Strata Java SE 8u40 or later is required.
The JAR files are available in [Maven Central](https://search.maven.org/search?q=g:com.opengamma.strata):

```
<dependency>
  <groupId>com.opengamma.strata</groupId>
  <artifactId>strata-measure</artifactId>
  <version>2.12.0</version>
</dependency>
```

The JAR files, along with the command line tool and examples, can also be obtained from
the [Strata Releases](https://github.com/OpenGamma/Strata/releases) page on GitHub.


Building Strata
---------------

The source code can be cloned using [git](http://git-scm.com/) from GitHub:

```
  git clone https://github.com/OpenGamma/Strata.git
```

The projects use [Apache Maven](https://maven.apache.org/) as the build system.
Version 3.5.0 or later is required.
Simply run this command to compile and install the source code locally:

```
  mvn install
```

That command publishes the built artifacts into your local Maven repository (`~/.m2/repository`
by default) under the mutable `2.12.74-SNAPSHOT` coordinates, so on a machine whose repository is
shared with other builds it replaces an input of theirs. If you only need the jars, `mvn package`
leaves them in each module's git-ignored `target/` and publishes nothing; if you want the `install`
lifecycle without the shared publication, add `-Dmaven.repo.local=<a directory of your own>`.

Strata is based on Java SE 8.
Our continuous integration regularly builds on both Java 8 and Java 11.
When using Java 8, version 8u40 or later is required due to bugs in earlier versions.
We do not recommend use of non-LTS releases, such as Java 9, 10 and 12 to 16.

The Strata examples project includes a GUI based on JavaFX.
On Java 8, this will be excluded from compilation if JavaFX is not available in the JDK.
On Java 11, OpenJFX is included as a jar file from Maven Central, so the GUI is always compiled.

We recommend builds of OpenJDK from providers other than Oracle, notably
[Amazon Corretto](https://aws.amazon.com/corretto/) and [Adoptium](https://adoptium.net/).

For more information about developing code on Strata
see the [documentation](https://strata.opengamma.io).


Scala port
----------

Alongside the Maven build, this repository contains a Scala 2.13 port of two Strata modules:
`strata-collect`, ported in the subset that `strata-basics` uses, and `strata-basics` itself.
The port is a separate, additional build, driven by [sbt](https://www.scala-sbt.org/) from the
repository root.
The Java modules under `modules/` are unchanged and are still built with Maven as described above.

To build the Scala port, JDK 21 and sbt 1.13.0 are required.
Scala 2.13.18 is resolved by the build itself, so it does not need to be installed separately.

Run this command to compile both Scala modules and run their test suites:

```
  sbt test
```

`strata-basics` is the root project of the sbt build and aggregates `strata-collect`,
so this single command covers both modules.

Run this command for the end-to-end demo:

```
  sbt "strata-basics/run"
```

The demo builds a `PeriodicSchedule`, adjusts its dates against a built-in `HolidayCalendar`
through an explicitly supplied `ReferenceData`, converts a `MultiCurrencyAmount` to another
currency through an `FxMatrix`, then serializes the results to JSON and prints them.

Run this script to check the port against its acceptance gates:

```
  scripts/verify-gates.sh
```

It is the single authoritative gate runner, intended to be run from a clean checkout on JDK 21.
Beyond the JDK and sbt it needs git, python3 and the POSIX text utilities.
It writes `target/gate-report.md` and exits non-zero if any automated gate fails.
The one gate that is a manual approval rather than a measurement is recorded as reported.

The [Scala migration note](SCALA_MIGRATION.md) records every ported `strata-collect` symbol with
its Scala replacement, and every deliberate divergence from the Java behaviour.
Each ported module also has its own README:
[Strata-Basics (Scala)](strata-basics/README.md) and
[Strata-Collect (Scala)](strata-collect/README.md).


Status
------

Strata is well-maintained, tested and functional.
It is used in production as the core of [OpenGamma SaaS Analytics](https://opengamma.com/).
The API will be maintained with backwards compatibility in mind.


Strata modules
--------------

Strata is formed from a number of modules:

* [Examples](examples/README.md)
* [Report](modules/report/README.md)
* [Measure](modules/measure/README.md)
* [Calc](modules/calc/README.md)
* [Loader](modules/loader/README.md)
* [Pricer](modules/pricer/README.md)
* [Market](modules/market/README.md)
* [Product](modules/product/README.md)
* [Data](modules/data/README.md)
* [Basics](modules/basics/README.md)
* [Collect](modules/collect/README.md)

The Scala port adds two modules, built with sbt rather than Maven:

* [Strata-Basics (Scala)](strata-basics/README.md)
* [Strata-Collect (Scala)](strata-collect/README.md)
