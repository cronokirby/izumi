[![Gitter](https://badges.gitter.im/7mind/izumi.svg)](https://gitter.im/7mind/izumi)
[![Patreon](https://img.shields.io/badge/patreon-sponsor-ff69b4.svg)](https://www.patreon.com/7mind)
[![Build Status](https://dev.azure.com/7mind/izumi/_apis/build/status/7mind.izumi?branchName=develop)](https://dev.azure.com/7mind/izumi/_build/latest?definitionId=1&branchName=develop)
[![codecov](https://codecov.io/gh/7mind/izumi/branch/develop/graph/badge.svg)](https://codecov.io/gh/7mind/izumi)
[![CodeFactor](https://www.codefactor.io/repository/github/7mind/izumi/badge)](https://www.codefactor.io/repository/github/7mind/izumi)
[![Latest Release](https://img.shields.io/github/tag/7mind/izumi.svg)](https://github.com/7mind/izumi/releases)
[![Maven Central](https://img.shields.io/maven-central/v/io.7mind.izumi/distage-core_2.12.svg)](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22io.7mind.izumi%22)
[![Sonatype releases](https://img.shields.io/nexus/r/https/oss.sonatype.org/io.7mind.izumi/distage-core_2.12.svg)](https://oss.sonatype.org/content/repositories/releases/io/7mind/izumi/)
[![Sonatype snapshots](https://img.shields.io/nexus/s/https/oss.sonatype.org/io.7mind.izumi/distage-core_2.12.svg)](https://oss.sonatype.org/content/repositories/snapshots/io/7mind/izumi/)
[![License](https://img.shields.io/github/license/7mind/izumi.svg)](https://github.com/7mind/izumi/blob/develop/LICENSE)
[![Latest version](https://index.scala-lang.org/7mind/izumi/latest.svg?color=orange)](https://index.scala-lang.org/7mind/izumi)
[![Awesome](https://cdn.rawgit.com/sindresorhus/awesome/d7305f38d29fed78fa85652e3a63e154dd8e8829/media/badge.svg)](https://github.com/lauris/awesome-scala)

[![Buy me a coffee](https://bmc-cdn.nyc3.digitaloceanspaces.com/BMC-button-images/custom_images/orange_img.png)](https://www.buymeacoffee.com/7mind)

What is it?
===========

Izumi (*jp. 泉, spring*) is a set of independent libraries and frameworks allowing you to significantly increase productivity of your Scala development.

including the following components:

1. [distage](https://izumi.7mind.io/latest/release/doc/distage/) – Staged, transparent and debuggable runtime & compile-time Dependency Injection Framework,
2. [logstage](https://izumi.7mind.io/latest/release/doc/logstage/) – Automatic structural logs from Scala string interpolations,
3. [idealingua](https://izumi.7mind.io/latest/release/doc/idealingua/) – API Definition, Data Modeling and RPC Language, optimized for fast prototyping – like gRPC, but with a human face. Currently generates servers and clients for Go, TypeScript, C# and Scala,
4. [Opinionated SBT plugins](https://izumi.7mind.io/latest/release/doc/sbt/) (moved to [7mind/sbtgen](https://github.com/7mind/sbtgen)) – Reduces verbosity of SBT builds and introduces new features – inter-project shared test scopes and BOM plugins (from Maven)
5. [Percept-Plan-Execute-Repeat (PPER)](https://izumi.7mind.io/latest/release/doc/pper/) – a pattern that enables modeling very complex domains and orchestrate deadly complex processes a lot easier than you're used to.

Docs
----

* [Documentation & Tutorials](https://izumi.7mind.io/latest/release/doc/)
* [Scaladoc](https://izumi.7mind.io/latest/release/api/)

Example projects:
* [DIStage Livecode from ScalaWAW Warsaw](https://github.com/7mind/distage-livecode)
* [Idealingua Example Project with TypeScript and Scala](https://github.com/7mind/idealingua-example)
* [DIStage Example Project (older)](https://github.com/7mind/distage-sample)

Slides:
* [ScalaUA Slides about DIStage](https://www.slideshare.net/7mind/scalaua-distage-staged-dependency-injection)
* [ScalaUA Slides about LogStage](https://www.slideshare.net/7mind/logstage-zerocosttructuredlogging)
* [Slides from other meetups](https://github.com/7mind/slides)

Video:
* [Youtube: DIStage Livecode](https://www.youtube.com/watch?v=C0srg5T0E4o&t=4971)

Key goals
=========

We aim to provide tools that:

1. Boost productivity and reduce code bloat
2. Are as non-invasive as possible
3. Are introspectable
4. Are better than anything else out there :3

Current state and future plans
==============================

We are looking for early adopters, contributors and sponsors.

This project is currently a work in progress.

In the future we are going to (or may) implement more tools based on PPER approach:

1. Best in the world build system
2. Best in the world cluster orchestration tool
3. Best in the world load testing/macro-benchmark tool

Credits
=======

[![YourKit](https://www.yourkit.com/images/yklogo.png)](https://www.yourkit.com)

YourKit supports open source projects with innovative and intelligent tools
for monitoring and profiling Java and .NET applications.
YourKit is the creator of [YourKit Java Profiler](https://www.yourkit.com/java/profiler/),
[YourKit .NET Profiler](https://www.yourkit.com/.net/profiler/) and
[YourKit YouMonitor](https://www.yourkit.com/youmonitor/).

[![Triplequote Hydra](https://triplequote.com/img/services/hydra-2.svg)](https://triplequote.com/)

[Triplequote Hydra](https://triplequote.com/) is the world’s only parallel compiler for the Scala language. Hydra works by parallelizing all of the Scala compiler phases, taking full advantage of the many cores available in modern hardware.

Contributors
============

* Run `./sbtgen.sc` to generate JVM-only sbt projects, run `./sbtgen.sc --js` to generate a JVM+JS sbt crossprojects

See:

- [Build notes](doc/md/build.md)
- [Project flow](doc/md/flow.md)

