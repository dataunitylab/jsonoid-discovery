# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Fixed
- Fix circular dependency with property sets

### Changed
- Disable assertions by default

## [0.30.1] - 2024-07-01
### Changed
- Update dependencies

## [0.30.0] - 2024-06-27
### Added
- Support `patternProperties` when converting to a dynamic object
- Enable SBOM in Docker build

### Fixed
- Fix some anomaly checking with `patternProperties`
- Fix some anomaly checking with `additionalProperties`

### Changed
- Update to Scala 2.13.14
- Update to Java 11
- Change base image for Docker container
- Set build timestamp for reproducible builds

## [0.20.1] - 2024-02-28
### Added
- Support for using `treeReduce` with Apache Spark

## [0.20.0] - 2024-02-20
### Fixed
- Fix array uniqueness subset check

### Changed
- Combine numeric schemas in `ProductSchema`
- Use JSON Pointer objects instead of strings

## [0.19.0] - 2023-11-26
### Fixed
- Fix package coordinates

## [0.18.0] - 2023-11-20
### Added
- Testing validation via Bowtie
- Collect anomalies for `patternProperties`

### Changed
- Throw error when parsing with `unevaluatedProperties`
- Set `items` to `false` in tuple schemas
- Declare lack of support for `unevaluatedItems` and `if`/`then`
- Declare partial support for `additionalProperties`
- Track Boolean values which are constant

## [0.17.0] - 2023-08-08
### Added
- Add calculation of schema entropy

### Changed
- Switch to draft 2020-12
- Switch back to `prefixItems`

## [0.16.0] - 2023-08-02
### Added
- Add dynamic object transformer
- Add disjoint key transformer
- Allow selecting a subset of properties by classes
- Allow removing subsets of properties
- Detect numeric strings and treat as numeric
- Added extended format checkers
- Apply `EnumTransformer` to constant Booleans
- Add test time assertions
- Add expansion to `ZeroSchema`
- Add some basic property-based testing

### Changed
- Increase length penalty for primary key detection
- Update to Scala 2.13
- Move property set configuration to params object
- Only generate tuple schemas on multiple observations
- Move transformers to separate package
- Allow top-level schemas that aren't objects
- Ignore invalid JSON input

### Fixed
- Allow schema replacement to replace the entire schema
- Correct histogram anomaly checking
- Avoid errors with very small `multipleOf` values
- Avoid errors with `multipleOf` and very different scales
- Don't anchor string matches in `StaticPatternProperty`
- Fix compatibility check with `ProductSchema`
- Don't crash with extremely large integer values
- Fix crash when merging numbers of different scales
- More complete merging with `ProductSchema`
- Better error handling when deserializing schemas
- Fix broken `isSubsetOf` for empty tuple schemas
- Fix broken `isSubsetOf` for empty `StringNumericProperty`
- Fix `isSubsetOf` in `DependenciesProperty`
- `AnySchema` should be a subset of itself
- Large `BigDecimal` values should be treated as extreme
- Add `JLong` to valid types for `IntegerSchema`
- Fix extreme value anomaly checks in histograms
- Fix tuple schema deserialization from JSON Schema

## [0.15.0] - 2023-04-11
### Changed
- Add namespace to `AnomalyLevel`
- Unknown properties are not anomalous if `additionalProperties` is true
- Make histogram bounds violation `Info` level
- Don't consider non-null values as anomalous with simple `NullSchema`

### Fixed
- Properly handle zero values in histogram anomaly checking
- Correct anomaly detection in `ProductSchema`
- More accurate anomaly detection for `PatternProperty`

## [0.14.0] - 2023-04-04
### Changed
- Move to new package name

### Fixed
- Fix JSONoid version in generated schema

## [0.13.0] - 2023-02-28
### Added
- Allow disabling expansion entirely
- Add possible debug output to CLI
- Add some compatibility checking for `StaticDependenciesProperty`

### Changed
- Refactor reading of JSON Schema from file

### Fixed
- Avoid crash during expansion if a property is missing
- Fix regex compatibility checking for `StaticPatternProperty`
- Properly report compatibility for `PatternProperty`
- Add missing `NumMultipleOfProperty` when converting `IntegerSchema`
- Make expansion actually work with large numbers
- Fix `multipleOf` compatibility checks with 0
- Correctly deal with negative multiples

### Changed
- Exit with non-zero status for invalid arguments
- Allow oblivious expansion with split discovery

## [0.12.2] - 2023-02-24
### Added
- Allow specifying random number seed for reproducible discovery

## [0.12.1] - 2023-02-23
### Added
- Add a property to track the percentage of true Boolean values
- Allow for oblivious expansion without another schema
- Add additionalProperties during oblivious expansion

### Changed
- Allow reset of min/max length for strings with format

## Fixed
- Fix missing bash in Docker image

## [0.12.0] - 2023-02-21
### Added
- Add equivalence relation which checks types
- Allow additional ER choices in CLI
- Allow the maximum number of examples to be configured
- Allow configuration of `additionalProperties`
- Allow checking of schema compatibility
- Expand schema properties to cover another schema where possible

### Changed
- Use ranking to improve possible primary key suggestions
- Use a configurable threshold for format detection, defaulting to 1
- Refactor reference replacement to allow replacing with any schema

## Fixed
- Don't output a format if most string values have no format
- Correctly check UUID format
- Correctly check email format

## [0.11.0] - 2023-01-13
### Added
- Make definition transformation optional when running via Spark
- Use [DDSketch](https://github.com/DataDog/sketches-java/) for histograms
- Build a Docker image on each push
- Include sbt wrapper script
- Add option to write schema directly to file
- Generate website with schema previews each push

## Fixed
- Don't output items in `ArraySchema` if empty
- Keep definitions on schema copy
- Don't fail in `DefinitionTransfer` on single keys

### Changed
- Refactor Bloom filters to reduce code duplication
- Added Scalafix along with some minor rewrites
- Pretty print final JSON schema

## [0.10.0] - 2022-04-15
### Added
- Add intersecting label equivalence relation
- Include serialized HLL in generated schema
- Allow limiting discovered properties from CLI

### Changed
- Require minimum examples for format property
- Include array item uniqueness and examples in simple property set

### Fixed
- Avoid discovering properties not required in CLI

## [0.9.7] - 2022-03-14
### Added
- Detect anomalies resulting from dependency violations

## [0.9.6] - 2022-03-12
### Fixed
- Fix nested property transformation

## [0.9.5] - 2022-03-12
### Fixed
- Fix property restriction for complex types

## [0.9.4] - 2022-03-11
### Added
- Allow restricting to a subset of properties

## [0.9.3] - 2022-03-09
### Fixed
- Fix Bloom filter deserialization

## [0.9.2] - 2022-03-09
### Added
- Fix `dependentRequired` during conversion

## [0.9.1] - 2022-03-09
### Added
- Support `dependentRequired` during conversion

## [0.9.0] - 2022-03-09
### Fixed
- Correct type anomaly detection

## [0.8.3] - 2022-03-08
### Fixed
- Avoid unnecessary anomaly errors on `patternProperties`

## [0.8.2] - 2022-03-08
### Added
- Serialize/deserialize Bloom filters
- Support `const` when converting schemas

## [0.8.1] - 2022-03-08
### Added
- Allow transformers to work on top-level objects
- Separate transformer to merge schemas containing `allOf`
- Anomaly detection for different `ProductSchema` types
- Parse examples when converting schemas

### Changed
- Add separate types for `ProductSchema`

### Fixed
- Properly detect type anomalies with nested schemas

## [0.8.0] - 2022-02-22
### Added
- Allow merging schemas by intersection instead of union
- Use a base schema in `ProductSchema`

### Fixed
- Restore CLI functionality
- Ensure special schemas are merged correctly

## [0.7.3] - 2022-02-16
### Fixed
- Don't show reference in `ReferenceObjectProperty#toString` to fix circular references
- Support circular references during reference resolution

## [0.7.2] - 2022-02-15
### Fixed
- Added missing `patternProperties` support to `SchemaWalker`

## [0.7.1] - 2022-02-15
### Added
- Support `patternProperties` during object conversion

## [0.7.0] - 2022-02-09
### Added
- Support exclusive min/max during conversion
- Support `allOf` during object conversion
- Track `multipleOf` for `NumberSchema`

### Changed
- Add more explicit object conversion errors
- `MultipleOfProperty` renamed to `IntMultipleOfProperty`

## [0.6.3] - 2022-02-08
### Added
- Perform heuristic type detection during object conversion
- Support `additionalItems` during object conversion

### Changed
- Use `AnySchema` during conversion when no type detected
- Allow `items` to be an array during object conversion

### Fixed
- Allow arrays to be converted without item type

## [0.6.2] - 2022-02-04
### Fixed
- Perform definition conversion for all types

## [0.6.1] - 2022-02-04
### Fixed
- Don't require root to be `ObjectSchema` when resolving references

## [0.6.0] - 2022-02-04
### Added
- Show error message when converting with `patternProperties`
- Include definitions when converting to JSONoid objects

### Changed
- Store definitions directly on the `JsonSchema` object, not as a property

## [0.5.5] - 2022-02-03
### Added
- Allow a reference object to be stored for schema references
- Correctly parse `$ref` during object conversion
- New `ReferenceResolver` which will add a property to `ReferenceSchema` with the path of the schema object

### Changed
- Enable Wartremover only for compilation (not tests)

## [0.5.4] - 2022-02-01
### Added
- Support cases where `type` is an array in JSONoid object conversion
- Support enums in JSONoid object conversion
- Add `format` support in JSONoid object conversion
- Add a separate `StaticPatternProperty` to statically specify regexes
- Handle `allOf` with a single element in JSONoid conversion
- Handle `true` and `false` in JSONoid object conversion

### Changed
- Make JSONoid conversion helper methods private
- Don't construct `ProductSchema` with a single element during object conversion

## [0.5.3] - 2022-01-31
### Changed
- Throw a more readable exception if `$ref` or `allOf` are found during conversion
- Assume something is object type during conversion if it has `properties`

### Fixed
- Allow objects with no defined properties when converting to JSONoid

## [0.5.2] - 2022-01-31
### Added
- `NonEquivalenceRelation` which never merges
- Also convert `anyOf` and `oneOf` to `ProductSchema` during conversion

## [0.5.1] - 2022-01-29
### Added
- New `getOrNone` method for `SchemaProperties` which returns an `Option`

### Changed
- Moved ``BuildInfo`` class to inside `discovery` package
- Update assembly JAR configuration to change name

## [0.5.0] - 2022-01-28
### Added
- Histograms are tracked for array and string lengths
- JSON values can be checked for anomalies against a schema
- JSON schemas can be converted to JSONoid objects

### Changed
- Better names for generated definitions

### Fixed
- Definition replacement now works for `ProductSchema`

## [0.4.2] - 2021-10-15
### Fixed
- Don't include `ProductSchema` counts in generated schema

## [0.4.1] - 2021-10-15
### Added
- Allow `ProductSchema` as a top-level result

### Fixed
- Better handling of clustering failures

## [0.4.0] - 2021-10-14
### Added
- Allow configurable equivalence relations when merging
- Add option to select whether definitions should be found
- Allow conversion of any type to a full JSON Schema
- Record count of different alternatives in `ProductSchema`

### Changed
- Use `oneOf` instead of `anyOf` in `ProductSchema`

## [0.3.0] - 2021-10-05
### Added
- Use clustering of related objects to automatically create definitions

### Fixed
- Prevented duplicate values from occurring in  `enum` in some circumstances
- Change `dependencies` property to `dependentRequired` to match latest schema version
- Change `prefixItems` back to `items` for now since we are using draft 2019

### Changed
- Produce 2019 draft version schemas
- Remove statistics from `NumberSchema` simple properties
- Rename `stats` property to `statistics`
- Separate elements of tuple schemas when generating value tables
- Add preliminary support for the definition and use of references

## [0.2.4] - 2021-08-27
## Fixed
- Approach to multiline strings for description generation was broken on some JREs

## [0.2.3] - 2021-08-27
### Added
- Add option for property sets to main CLI
- Warning message in generated schema description

### Changed
- Include version number in CLI output

## [0.2.2] - 2021-08-24
### Changed
- Removed `$vocabulary` from the generated schema

## [0.2.1] - 2021-08-24
### Added
- Add class to directly run JSONoid on Spark
- Add different sets of properties which can be discovered with Spark

### Changed
- Version number (incorrect) removed from CLI output

## [0.2.0] - 2021-06-15
### Changed
- `PrimaryKeyFinder` now searches recursively and returns JSON Paths
- Generate schema using 2020-12 spec
- Tuple schemas are now produced using `prefixItems`

### Fixed
- Arrays now correctly use `minItems` and `maxItems` instead of `minLength` and `maxLength`
- `multipleOf` is not included if the multiple is zero (only happens for constants)

## [0.1.3] - 2021-06-09
### Added
- Subschemas can now be extracted using a JSON pointer string

### Fixed
- Reservoir sampling for example collection now actually uses new examples

## [0.1.2] - 2021-06-08
### Added
- Also detect suffix patterns in strings

## [0.1.1] - 2021-06-01
### Added
- Method for conversion to simple `JObject` JSON Schema

## [0.1.0] - 2021-05-31
- Initial release

[Unreleased]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.30.1...HEAD
[0.30.1]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.30.0...v0.30.1
[0.30.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.20.1...v0.30.0
[0.20.1]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.19.0...v0.20.1
[0.20.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.19.0...v0.20.0
[0.19.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.18.0...v0.19.0
[0.18.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.17.0...v0.18.0
[0.17.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.16.0...v0.17.0
[0.16.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.15.0...v0.16.0
[0.15.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.14.0...v0.15.0
[0.14.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.13.0...v0.14.0
[0.13.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.12.2...v0.13.0
[0.12.2]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.12.1...v0.12.2
[0.12.1]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.12.0...v0.12.1
[0.12.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.11.0...v0.12.0
[0.11.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.10.0...v0.11.0
[0.10.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.9.7...v0.10.0
[0.9.7]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.9.6...v0.9.7
[0.9.6]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.9.5...v0.9.6
[0.9.5]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.9.4...v0.9.5
[0.9.4]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.9.3...v0.9.4
[0.9.3]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.9.2...v0.9.3
[0.9.2]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.9.1...v0.9.2
[0.9.1]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.9.0...v0.9.1
[0.9.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.8.3...v0.9.0
[0.8.3]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.8.2...v0.8.3
[0.8.2]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.8.1...v0.8.2
[0.8.1]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.8.0...v0.8.1
[0.8.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.7.3...v0.8.0
[0.7.3]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.7.2...v0.7.3
[0.7.2]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.7.1...v0.7.2
[0.7.1]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.7.0...v0.7.1
[0.7.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.6.3...v0.7.0
[0.6.3]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.6.2...v0.6.3
[0.6.2]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.6.1...v0.6.2
[0.6.1]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.6.0...v0.6.1
[0.6.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.5.5...v0.6.0
[0.5.5]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.5.4...v0.5.5
[0.5.4]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.5.3...v0.5.4
[0.5.3]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.5.2...v0.5.3
[0.5.2]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.5.1...v0.5.2
[0.5.1]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.5.0...v0.5.1
[0.5.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.4.2...v0.5.0
[0.4.2]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.4.1...v0.4.2
[0.4.1]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.2.4...v0.3.0
[0.2.4]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.2.3...v0.2.4
[0.2.3]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.1.3...v0.2.0
[0.1.3]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/dataunitylab/jsonoid-discovery/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/dataunitylab/jsonoid-discovery/releases/tag/v0.1.0
