# Changelog
All notable changes to this project will be documented in this file.

Till version `0.1.0`, each patch version may be breaking change.

## [Unreleased]

### Added
- 

### Changed
- Allowed `Option<T>` on element attributes.

### Deprecated
- 

### Removed
- 

### Fixed
- 

### Security
- 

## [0.0.3] - 2018-10-08

### Added
- Detachable state setters using `StateSetter::state_setter` & `SetState::set_state`.

### Changed
- Made `Shared` type private.
- Batch state changes into one.
- Upgraded to `rustc 1.31.0-nightly (4efdc04a5 2018-10-06)`.

### Fixed
- `'static` prop types were treated as optional when they are not.

## [0.0.2] - 2018-10-03

### Added
- First release.