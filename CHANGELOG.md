# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog][changelog], and this project adheres
to the [Haskell Package Versioning Policy][pvp].

## [Unreleased]

- Send `MVar` instead of `Unagi.Chan` in `Envelope`
- Allow using `call` with response type of `()`

  So that you can send synchronous/blocking messages that don't have a
  meaningful response. Useful if you need to wait until a message is handled
  before proceeding.

## [0.3.0.0] - 2021-02-23

### Added

- Added `cast`, `call`, and `handle` functions, and `Envelope` message wrapper
type, which enforce you get the response you expect from certain messages
- Added convenient `Server` type alias for processes using `Envelope`s
- Added `examples/use.hs` to experiment with "use pattern"
- Moved things to separate modules (everything still re-exported from top-level
`Drama` module)
- Added `NoMsg`, to be used instead of `Void`

### Changed

- Renamed "actor" to "process"
- Renamed `Message` to `HasMsg`
- Renamed `exit` to `stop`
- Generalized `loop`, `continue`, and `stop` to any monad
- Updated examples to use `cast` and `call` where possible
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

[Unreleased]: https://github.com/evanrelf/drama/compare/v0.3.0.0...HEAD
[0.3.0.0]: https://github.com/evanrelf/drama/releases/tag/v0.3.0.0
[0.2.0.0]: https://github.com/evanrelf/drama/releases/tag/v0.2.0.0
[0.1.0.3]: https://github.com/evanrelf/drama/releases/tag/v0.1.0.3
[0.1.0.2]: https://github.com/evanrelf/drama/releases/tag/v0.1.0.2
[0.1.0.1]: https://github.com/evanrelf/drama/releases/tag/v0.1.0.1
[0.1.0.0]: https://github.com/evanrelf/drama/releases/tag/v0.1.0.0

[changelog]: https://keepachangelog.com/en/1.0.0/
[pvp]: https://pvp.haskell.org/
