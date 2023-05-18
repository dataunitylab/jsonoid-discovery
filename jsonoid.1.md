% JSONOID(1) jsonoid
% Michael Mior

# NAME

**jsonoid** - schema discovery for JSON using monoids

# SYNOPSIS

| **jsonoid** \[*OPTIONS*] *input.jsonl*
| **jsonoid** \[**\--help**]

# DESCRIPTION

jsonoid is a schema discovery tool for JSON that makes use of monoids to make
discovery scalable and efficient.

# OPTIONS

\--help

: Print a help message.

-w, \--write-output

: Where to write the generated schema, defaults to stdout.

-p, \--prop

: The property set that should be used during discovery. This selects the set
of monoids jsonoid will use during the discovery process and affects both the
level of detail in the generated schema and the runtime of the discovery
process. There are three pre-defined property sets:

      - All (default) - use all available monoids for discovery
      - Simple - use any monoid corresponding to a property in the JSON Schema standard
      - Min - the smallest possible set of properties containing basic type information

-e, \--equivalence-relation

: The equivalence relation to use when merging schemas. This also affects the
level of detail in the generated schema by determining which objects are
combined together. There are four pre-defined equivalence relations:

      - Kind (default) - combine any schemas of the same basic type (e.g., object)
      - Label - combine any schemas which have the same set of keys
      - IntersectingLabel - combine any schemas which have keys in common
      - TypeMatch - combine any schemas with keys where the values are the same type

-d, \--add-definitions

: Attempt to add definitions to the generated schema based on commonly
occurring sets of properties. This will replace each discovered set with a
reference to a definition.

-y, \--detect-dynamic

: Attempt to discover "dynamic" objects in the generated schema. These are
objects where the keys are actually data values. The individual keys are then
not included in the generated schema, but `additionalProperties` is used to
define the type of all values (which must be the same). Consider the example
JSON below:

    ```json
    {"population": {"Rochester": 210606, "Boston": 654776}}
    ```

    With dynamic object detection, the schema would be similar to:

    ```json
    {"type": "object", "additionalProperties": {"type": "integer"}}
    ```

\--numeric-strings

: Attempt to detect numeric strings in the generated schema. These are numeric
values which are given the string type in JSON. With this option enabled, these
values will be given type `number` or `integer` instead of `string`. It is the
responsibility of the parser to output these values as numbers.

\--max-examples

: The maximum number of examples to collect for any leaf value (default 100).

-a, \--additional-properties

: Whether to set `additionalProperties` to `true` in the generated schema. This
defines whether the generated schema is "open" or "closed". An open schema
(with `additionalProperties` set to `true`) will accept keys which were not
seen during the discovery process, while a closed schema will not.

\--format-threshold

: The fraction of values which must match a discovered `format` value for
strings to consider the format valid.

-s, \--split-percentage

: Enable split discovery mode. This operates by discovering a schema on a
percentage of documents (default 90%), and then expanding this schema to cover
the schema discovered from the remainder of the documents. The difference as
compared to generating a schema using all documents is that the expansion
process makes fewer assumptions as to the content of the documents. Thus,
split discovery may generalize better to documents outside of those used during
the discovery process.

\--never-expand

: Never expand the generated schema. This is mostly useful when using split
discovery mode to skip the expansion step. The resulting schema will then be
the schema generated from the fraction of sampled documents.

\--oblivious-expansion

: This performs expansion on the discovered schema similar to the split
discovery mode. However, with oblivious expansion, there are no documents used
to guide the expansion process. The resulting schema is simply made somewhat
more accepting (e.g., the maximum length of strings is increased).

\--reset-format-length

: Reset the minimum and maximum length of strings with an applied `format`.
Length restrictions are often less useful for such strings and can be more
restrictive than necessary.

\--random-seed

: Set the seed for the random number generator to ensure reproducible results.
This is particularly useful when using the split discovery mode.

-o, \--only-properties

: Limit the set of properties used during discovery to the given list. This
should be a comma-separate list of property names. First, the default property
set is used, then any properties not in this list are excluded.

-v, \--values

: A file path where a table of values should be written. This is mostly useful
for testing purposes and debugging.

\--debug

: Enable debug mode. Specifically, this will produce more verbose output and
in some cases also save intermediate output to file for later inspection.
