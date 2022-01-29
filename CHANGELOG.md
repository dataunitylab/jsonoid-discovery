# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
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

[Unreleased]: https://github.com/michaelmior/jsonoid-discovery/compare/v0.5.0...HEAD
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
