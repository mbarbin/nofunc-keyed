## Unreleased

### Changed

- In `Map` and `Hashtbl` (the non-`std` packages), swapped the names `find` and `find_opt`: `find` now returns an `option` and `find_opt` is gone; the raising version is now `find_exn` (@mbarbin).
- In `Set` and `Hashset` (the non-`std` packages), `fold`'s callback now labels the element `~elt` instead of `~key` - these are element containers, not key/value ones, so `key` didn't fit (@mbarbin).
- In `Set`, `Map`, `Hashtbl` and `Hashset` (the non-`std` packages), `fold`'s accumulator is now labeled `~init` instead of positional - it was an oversight that it wasn't, unlike every other optional-looking argument in this API (@mbarbin).
- Moved `test/` from `nofunc-keyed-dev` to a new dedicated `nofunc-keyed-tests` package (@mbarbin).

## 0.1.0 (2026-02-19)

This initial release includes only the `std` flavor of the distribution with other variants left as future work.

### Added

- Added tests (#11, #12, @mbarbin).
- Add `std`-flavored `Map`, `Set`, `Hashtbl`, `Hashset`. (#3, #7, #8, @mbarbin).
