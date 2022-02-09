# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.7.0]
### Added
- Support exclusive min/max during conversion
- Support `allOf` during object conversion
- Track `multipleOf` for `NumberSchema`

### Changed
- Add more explicit object conversion errors
- `MultipleOfProperty` renamed to `IntMultipleOfProperty`

## [0.6.3]
### Added
- Perform heuristic type detection during object conversion
- Support `additionalItems` during object conversion

### Changed
- Use `AnySchema` during conversion when no type detected
- Allow `items` to be an array during object conversion

### Fixed
- Allow arrays to be converted without item type

## [0.6.2]
### Fixed
- Perform definition conversion for all types

## [0.6.1]
### Fixed
- Don't require root to be `ObjectSchema` when resolving references

## [0.6.0]
### Added
- Show error message when converting with `patternProperties`
- Include definitions when converting to JSONoid objects

### Changed
- Store definitions directly on the `JsonSchema` object, not as a property

## [0.5.5]
### Added
- Allow a reference object to be stored for schema references
- Correctly parse `$ref` during object conversion
- New `ReferenceResolver` which will add a property to `ReferenceSchema` with the path of the schema object

### Changed
- Enable Wartremover only for compilation (not tests)

## [0.5.4]
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

## [0.5.3]
### Changed
- Throw a more readable exception if `$ref` or `allOf` are found during conversion
- Assume something is object type during conversion if it has `properties`

### Fixed
- Allow objects with no defined properties when converting to JSONoid

## [0.5.2]
### Added
- `NonEquivalenceRelation` which never merges
- Also convert `anyOf` and `oneOf` to `ProductSchema` during conversion

## [0.5.1]
### Added
- New `getOrNone` method for `SchemaProperties` which returns an `Option`

### Changed
- Moved ``BuildInfo`` class to inside `discovery` package
- Update assembly JAR configuration to change name

## [0.5.0]
### Added
- Histograms are tracked for array and string lengths
- JSON values can be checked for anomalies against a schema
- JSON schemas can be converted to JSONoid objects

### Changed
- Better names for generated definitions

### Fixed
- Definition replacement now works for `ProductSchema`

## [0.4.2]
### Fixed
- Don't include `ProductSchema` counts in generated schema

## [0.4.1]
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

[Unreleased]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.7.0...HEAD
[0.7.0]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.6.3...v0.7.0
[0.6.3]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.6.2...v0.6.3
[0.6.2]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.6.1...v0.6.2
[0.6.1]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.6.0...v0.6.1
[0.6.0]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.5.5...v0.6.0
[0.5.5]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.5.4...v0.5.5
[0.5.4]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.5.3...v0.5.4
[0.5.3]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.5.2...v0.5.3
[0.5.2]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.5.1...v0.5.2
[0.5.1]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.5.0...v0.5.1
[0.5.0]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.4.2...v0.5.0
[0.4.2]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.4.1...v0.4.2
[0.4.1]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.2.4...v0.3.0
[0.2.4]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.2.3...v0.2.4
[0.2.3]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.1.3...v0.2.0
[0.1.3]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/michaelmior/jsonoid-discovery/releases/tag/v0.1.0
