# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Changed
- `PrimaryKeyFinder` now searches recursively and returns JSON Paths

### Fixed
- Arrays now correctly use `minItems` and `maxItems` instead of `minLength` and `maxLength`

## [v0.1.3] - 2021-06-09
### Added
- Subschemas can now be extracted using a JSON pointer string

### Fixed
- Reservoir sampling for example collection now actually uses new examples

## [v0.1.2] - 2021-06-08
### Added
- Also detect suffix patterns in strings

## [v0.1.1] - 2021-06-01
### Added
- Method for conversion to simple `JObject` JSON Schema

## [v0.1.0] - 2021-05-31
- Initial release
