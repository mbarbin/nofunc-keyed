## Unreleased

### Added

- Added `Hashset.of_list` (@mbarbin).
- Added `Hashtbl.set` and `Hashtbl.set_seq` (the non-`std` package), aliases for `replace` and `replace_seq`, which are now deprecated with `ocamlmig` migration annotations (@mbarbin).
- Added `Hashtbl.shadow` and `Hashtbl.shadow_seq` (the non-`std` package), renamed from `add` and `add_seq`, which are now deprecated with `ocamlmig` migration annotations. The rename makes this rarer operation (it shadows rather than replaces an existing binding) less likely to be reached for by mistake - most callers meaning to bind a key regardless of its previous value should use `set` instead (@mbarbin).
- Added `Hashtbl.remove_all` (the non-`std` package): unlike `remove`, which only pops the most recently shadowed binding of a key, `remove_all` clears every binding of that key at once (@mbarbin).
- Added `is_empty` to `Hashtbl` and `Hashset` (the non-`std` packages), matching the `is_empty` already present in `Set` and `Map` (@mbarbin).

### Changed

- In `Map` and `Hashtbl` (the non-`std` packages), swapped the names `find` and `find_opt`: `find` now returns an `option` and `find_opt` is gone; the raising version is now `find_exn` (@mbarbin).
- In `Set` and `Hashset` (the non-`std` packages), `fold`'s callback now labels the element `~elt` instead of `~key` - these are element containers, not key/value ones, so `key` didn't fit (@mbarbin).
- In `Set`, `Map`, `Hashtbl` and `Hashset` (the non-`std` packages), `fold`'s accumulator is now labeled `~init` instead of positional - it was an oversight that it wasn't, unlike every other optional-looking argument in this API (@mbarbin).
- Moved `test/` from `nofunc-keyed-dev` to a new dedicated `nofunc-keyed-tests` package (@mbarbin).

### Fixed

- `Set.compare` and `Map.compare` (the non-`std` packages) now return `Ordering.t` instead of `int`, and `Map.compare`'s `f` (comparing associated data) now takes an `Ordering.t`-returning function too. These were an oversight - every other comparator in the non-`std` API already used `Ordering.t`, but these two whole-container "total ordering" functions were left returning plain `int`, inherited unconverted from the OCaml stdlib (@mbarbin).

### Deprecated

- `Hashtbl.replace` and `Hashtbl.replace_seq` (the non-`std` package) are deprecated in favor of `Hashtbl.set` and `Hashtbl.set_seq`. Run `ocamlmig migrate` to update call sites (@mbarbin).
- `Hashtbl.add` and `Hashtbl.add_seq` (the non-`std` package) are deprecated in favor of `Hashtbl.shadow` and `Hashtbl.shadow_seq`. Run `ocamlmig migrate` to update call sites (@mbarbin).

## 0.1.0 (2026-02-19)

This initial release includes only the `std` flavor of the distribution with other variants left as future work.

### Added

- Added tests (#11, #12, @mbarbin).
- Add `std`-flavored `Map`, `Set`, `Hashtbl`, `Hashset`. (#3, #7, #8, @mbarbin).
