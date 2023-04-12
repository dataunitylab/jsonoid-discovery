# JSONoid Discovery

[![CI](https://github.com/dataunitylab/jsonoid-discovery/actions/workflows/ci.yml/badge.svg)](https://github.com/dataunitylab/jsonoid-discovery/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/dataunitylab/jsonoid-discovery/branch/main/graph/badge.svg?token=VhcWABnQhU)](https://codecov.io/gh/dataunitylab/jsonoid-discovery)

Schema discovery for JSON Schema draft 2019-09 using monoids.
The goal of JSONoid is to produce a useful JSON Schema from a collection of JSON documents.
For an idea of what JSONoid does, you can view [example schemas with their corresponding datasets](https://dataunitylab.github.io/jsonoid-discovery/schemas/).

Currently this package uses unreleased code via GitHub Packages.
Although all the code is public, you must [create a personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token).
This token should be stored in a file named `.env` in the root of the repository with a line `GITHUB_TOKEN=YOUR_TOKEN_HERE`.
Only the `read:packages` permission should be required.

## Running

To quickly run jsonoid, you can use the Docker image which is built from the latest commit on the `main` branch.
Note that by default, jsonoid accepts [newline-delimited JSON](http://ndjson.org/) on standard input, so it will hang waiting for input.
Add the `--help` option to see possible configuration options.

    docker run -i --rm michaelmior/jsonoid-discovery

To simplify, you may wish to add a shell alias so `jsonoid` can be run directly as a command.

    alias jsonoid='docker run -i --rm michaelmior/jsonoid-discovery
    jsonoid --help

## Compiling

To produce a JAR file which is suitable for running either locally or via Spark, run `sbt assembly`.
This requires an installation of [sbt](https://www.scala-sbt.org/).
Alternatively, you can use `./sbtx assembly` to attempt to automatically download install the appropriate sbt and Scala versions using [sbt-extras](https://github.com/dwijnand/sbt-extras).
This will produce a JAR file under `target/scala-2.13/` which can either be run directly or passed to `spark-submit` to run via Spark.

## Schema monoids

In JSONoid, the primary way information is collected from a schema is using [monoids](https://en.wikipedia.org/wiki/Monoid).
A monoid simply stores a piece of information extracted from a JSON document along with information on how to combine together information from all documents in a collection in a scalable way.

The set of monoids (also referred as properties) used for discovery can be controlled using the `--prop` command line option.
The `Min` set of monoids will produce only simple type information and nothing more.
`Simple` extends this set of monoids to cover a large set of keywords supported by JSON Schema.
Finally, `All` monoids can be enabled to discover the maximum amount of information possible.
Note that for large collections of documents, there may be a performance penalty for using all possible monoids in the discovery process.

For each primitive type, the following monoids are defined.

- `BloomFilter` - A [Bloom filter](https://en.wikipedia.org/wiki/Bloom_filter) allows for approximate membership testing. The Bloom filters generated are a Base64 encoded serialized [library object](https://github.com/michaelmior/bloomfilter/).
- `Examples` - Corresponding to the `examples` JSON Schema keyword, a number of example values will be randomly sampled from the observed documents.
- `HyperLogLog` - [HyperLogLog](https://en.wikipedia.org/wiki/HyperLogLog) allows estimates of the number of unique values of a particular key. As with Bloom filters, the generated value is a Base64 encoded [library object](https://github.com/prasanthj/hyperloglog).

### Arrays

- `Histogram`, `MaxItems`, `MinItems` - Produces a histogram of array size and the maximum and minimum number of elements.
- `Unique` - Detects whether elements of an array are unique corresponding to the [`uniqueItems`](https://json-schema.org/understanding-json-schema/reference/array.html#uniqueness) JSON Schema keyword.

### Numbers (integer and decimal)

- `Histogram`, `MaxValue`, `MinValue` - A histogram of all values and the maximum and minimum values.
- `MultipleOf` - If all numerical values are a multiple of a particular constant, this will be detected using Euclid's GCD algorithm. The corresponds to the JSON Schema [`multipleOf`](https://json-schema.org/understanding-json-schema/reference/numeric.html#multiples) keyword.
- `Stats` - Several statistical properties including mean, standard deviation, skewness, and kurtosis are calculated.

### Objects

- `Dependencies` - In some schemas, a key must exist an object if some other key exists, as in the JSON Schema [`dependentRequired`](https://json-schema.org/understanding-json-schema/reference/conditionals.html?highlight=depend#dependentrequired) keyword. For example, if a `city` is provided, it may also be necessary to provide a `state`.
- `FieldPresence` - For keys which are not required, this tracks the percentage of objects which contain this property.
- `Required` - This tracks which keys are always present in a schema, suggesting that they are required.

### Strings

- `Format` - This attempts to infer a value for the [`format`](https://json-schema.org/understanding-json-schema/reference/string.html#built-in-formats) keyword. Formats are semantic types of strings such as URLs or email addresses. A string will be labelled with the most common format detected.
- `LengthHistogram`, `MaxLength`, `MinLength` - Both the minimum and maximum length of strings as well as a histogram of all string lengths will be included.
- `Format` - This attempts to infer a value for the [`pattern`](https://json-schema.org/understanding-json-schema/reference/string.html#regular-expressions) keyword. A pattern is a regular expression which all string values must match. Currently this property simply finds common prefixes and suffixes of strings in the schema.

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
