# Changelog

All notable changes.

Issue references of the form #<number> refer to tickets on Github: https://github.com/equill/restagraph/issues


## [0.2.1]

### Bugs fixed

- Remove flakiness from a test for autocommit transactions, by making it insensitive to the order of results returned from the server.


### Added

- #5: Non-trivial negative integers are now supported.
    - Both encoding and decoding are supported.
    - Tests cover encoding, decoding and round-tripping through the database.


### Changed

- `README.md` has been updated to reflect the current state of things.


### Removed

- #13: HTTP API support
    - The code itself.
    - Tests.
    - Import of all dependencies specific to this implementation.
    - Any mention of it in the API.


### Security

- No security issues addressed in this release.


### Deprecated

- Deprecation of the HTTP API is complete, by way of its removal.
