# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog][changelog], and this project adheres
to the [Haskell Package Versioning Policy][pvp].

## [Unreleased]

## [1.0.0.0] - 2021-02-21

### Added

- Added `Server` abstraction (enforces replies with `call`)
- Added `examples/server.hs` to show use of `Server`
- Moved things to separate modules (everything still re-exported from top-level
`Drama` module)
- Added `NoMsg`, to be used instead of `Void`

### Changed

- Renamed "actor" to "process"
- Renamed `Message` to `HasMsg`
- Generalized `loop`, `continue`, and `exit` to any monad
- Updated `workers` example to use `call` function
- Improved error message for unreachable `msg ~ Void` state

## Fixed

- Fixed incorrect use of `-XCPP` with `deriving newtype MonadFail`

## [0.2.0.0] - 2021-02-20

### Added

- Added more efficient `spawn_` and `run_` functions for spawning actors that
don't receive messages
- Re-exported common functions and types, such as `MonadIO` and `forever`, via
`Drama.Reexports` module

### Changed

- Added `Message` constraint to functions dealing with an actor's `Address` and
`Mailbox` (`send`, `receive`, `spawn`, etc.)

## [0.1.0.3] - 2021-02-20

### Added

- Examples now have their own executable components, separate from the library

### Removed

- Removed `SharedResource` and `Workers` modules from library

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

[Unreleased]: https://github.com/evanrelf/drama/compare/v1.0.0.0...HEAD
[1.0.0.0]: https://github.com/evanrelf/drama/releases/tag/v1.0.0.0
[0.2.0.0]: https://github.com/evanrelf/drama/releases/tag/v0.2.0.0
[0.1.0.3]: https://github.com/evanrelf/drama/releases/tag/v0.1.0.3
[0.1.0.2]: https://github.com/evanrelf/drama/releases/tag/v0.1.0.2
[0.1.0.1]: https://github.com/evanrelf/drama/releases/tag/v0.1.0.1
[0.1.0.0]: https://github.com/evanrelf/drama/releases/tag/v0.1.0.0

[changelog]: https://keepachangelog.com/en/1.0.0/
[pvp]: https://pvp.haskell.org/
