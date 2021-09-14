# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased
## Changed
- Produce 2019 draft version schemas
- Remove statistics from `NumberSchema` simple properties
- Rename `stats` property to `statistics`

## Fixed
- Change `dependencies` property to `dependentRequired` to match latest schema version
- Change `prefixItems` back to `items` for now since we are using draft 2019

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
