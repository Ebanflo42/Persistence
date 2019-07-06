# Revision history for Persistence

## 1.0  -- 2018-05-11

* First version. Released on an unsuspecting world.

## 1.1  -- 2018-05-27

### Added
- Bottleneck distance, a way to compare the topology of different data sets.
- HasseDiagram module, will allow users to deduce topological features of information flow in networks.
- A function for encoding generic graphs as 2D vectors.

### Changed
- Improved documentation for all exposed modules.

## 1.1.3 -- 2018-07-30

### Changed

- Fixed a major bug with persistent homology; high dimensional holes were being detected in low dimensional data sets.
- Persistent homology now filters out bar codes of the form (i, Just i), as they say nothing about the topology of the underlying complexes.

## 1.1.4 -- 2018-09-15

### Changed

- Fixed spelling error.
- Persistence now exports the constructors for `Extended a`.

## 1.1.4.1 -- 2018-09-15

### Changed

- Fixed all spelling errors, should actually build now.

## 1.1.4.2 -- 2018-09-15

### Changed

- Fixed non-exhaustive pattern match in `BottleNeckDistance` functions.

## 2.0 -- To be determined

### Changed

- The module `Persistence` has been renamed to `Filtration,` and all modules now exist within `Persistence`.

- Bar codes now take a type as a parameter so that they can not only represent the filtration indices at which features appear but also the scales.

- Persistent homology functions now take data sets as either vectors or lists.

- Fixed minor issue with bottleneck distance and removed the unsafe functions.

- Improved documentation for all exposed modules

- `Extended` type now exports its constructors and its instance os `Show`.

### Added

- Simplex type synonyms for making other type synonyms and signatures more readable.

- Data structures for filtrations both with and without all vertices having filtration index equal to zero, and persistent homology functions for processing each structure.

- Persistent homology functions that return Bar codes in terms of scales.

- Functions for constructing and manipulating persistence landscapes.