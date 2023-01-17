# JSONoid Discovery

[![CI](https://github.com/michaelmior/jsonoid-discovery/actions/workflows/ci.yml/badge.svg)](https://github.com/michaelmior/jsonoid-discovery/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/michaelmior/jsonoid-discovery/branch/main/graph/badge.svg?token=VhcWABnQhU)](https://codecov.io/gh/michaelmior/jsonoid-discovery)

Schema discovery for JSON Schema draft 2019-09 using monoids.
The goal of JSONoid is to produce a useful JSON Schema from a collection of JSON documents.
For an idea of what JSONoid does, you can view [example schemas with their corresponding datasets](https://michaelmior.github.io/jsonoid-discovery/schemas/).

Currently this package uses unreleased code via GitHub Packages.
Although all the code is public, you must [create a personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token).
This token should be stored in a file named `.env` in the root of the repository with a line `GITHUB_TOKEN=YOUR_TOKEN_HERE`.
Only the `read:packages` permission should be required.

## Running

To quickly run jsonoid, you can use the Docker image which is built from the latest commit on the `main` branch.
Note that by default, jsonoid accepts newline-delimited JSON on standard input, so it will hang waiting for input.
Add the `--help` option to see possible configuration options.

    docker run -i --rm ghcr.io/michaelmior/jsonoid-discovery:latest

To simplify, you may wish to add a shell alias so `jsonoid` can be run directly as a command.

    alias jsonoid='docker run -i --rm ghcr.io/michaelmior/jsonoid-discovery:latest'
    jsonoid --help

## Compiling

To produce a JAR file which is suitable for running either locally or via Spark, run `sbt assembly`.
This requires an installation of [sbt](https://www.scala-sbt.org/).
Alternatively, you can use `./sbtx assembly` to attempt to automatically download install the appropriate sbt and Scala versions using [sbt-extras](https://github.com/dwijnand/sbt-extras).
This will produce a JAR file under `target/scala-2.11/` which can either be run directly or passed to `spark-submit` to run via Spark.

## Equivalence relations

The concept of equivalence relations was first introduced by Baazizi et al. in [Parametric schema inference for massive JSON datasets](https://link.springer.com/article/10.1007/s00778-018-0532-7.)
The idea is that some JSON Schemas may contain some level of variation such as optional values and multiple possible types for a given key.
Whether any particular schemas should be considered equivalent is dependent on the particular dataset in question, so this equivalence is configurable.

JSONoid currently supports four equivalence relations (which can be specified using the `--equivalence-relation` command line option):

1. **Kind** equivalence (the default) will combine schemas when they are of the same kind, e.g. both objects, regardless of the contents of the objects.

2. **Label** equivalence will combine object schemas only if they have the same keys, regardless of the value of the key.

3. **IntersectingLabel** equivalence will combine object schemas if they have any keys in common. This can be helpful when some keys are optional since label equivalence would consider two schemas as different if one is missing an optional key.

4. **TypeMatch** equivalence will combine object schemas if any keys that they have in common have the same type. Note that this equivalence is *shallow*, meaning that two values are considered the same type if they are both objects or arrays, without considering the contained types (similar to **kind** equivalence).

## Datasets

* [GDP](http://api.worldbank.org/countries/USA/indicators/NY.GDP.MKTP.CD?per_page=5000&format=json)
* [Nobel Prize](http://api.nobelprize.org/v1/prize.json)
* [USGS - Earthquakes](https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson)
* [Rick and Morty characters](https://rickandmortyapi.com/api/character/)
* [TVmaze](https://api.tvmaze.com/singlesearch/shows?q=mr-robot&embed=episodes)
