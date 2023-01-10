# JSONoid Discovery

[![CI](https://github.com/michaelmior/jsonoid-discovery/actions/workflows/ci.yml/badge.svg)](https://github.com/michaelmior/jsonoid-discovery/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/michaelmior/jsonoid-discovery/branch/main/graph/badge.svg?token=VhcWABnQhU)](https://codecov.io/gh/michaelmior/jsonoid-discovery)

Schema discovery for JSON Schema draft 2019-09 using monoids.

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

## Datasets

* [GDP](http://api.worldbank.org/countries/USA/indicators/NY.GDP.MKTP.CD?per_page=5000&format=json)
* [Nobel Prize](http://api.nobelprize.org/v1/prize.json)
* [USGS - Earthquakes](https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson)
* [Rick and Morty characters](https://rickandmortyapi.com/api/character/)
* [TVmaze](https://api.tvmaze.com/singlesearch/shows?q=mr-robot&embed=episodes)
