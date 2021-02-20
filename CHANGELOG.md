# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog][changelog], and this project adheres
to the [Haskell Package Versioning Policy][pvp].

## [Unreleased]

## [0.1.0.2] - 2021-02-20

### Added

- Added examples (only compiled when `examples` flag is enabled)
- Added `CHANGELOG.md`, `LICENSE`, and `README.md` to `extra-source-files`

### Changed

- Applied suggestions from HLint

### Removed

- Removed `Drama.Demo` module

## [0.1.0.1] - 2021-02-19

### Fixed

- Fixed build failing on GHC 8.6.5 due to missing `MonadFail` type class

### Changed

- Relaxed upper bound on `transformers` library

## [0.1.0.0] - 2021-02-19

Initial release

[Unreleased]: https://github.com/evanrelf/drama/compare/v0.1.0.2...HEAD
[0.1.0.1]: https://github.com/evanrelf/drama/releases/tag/v0.1.0.2
[0.1.0.1]: https://github.com/evanrelf/drama/releases/tag/v0.1.0.1
[0.1.0.0]: https://github.com/evanrelf/drama/releases/tag/v0.1.0.0

[changelog]: https://keepachangelog.com/en/1.0.0/
[pvp]: https://pvp.haskell.org/
