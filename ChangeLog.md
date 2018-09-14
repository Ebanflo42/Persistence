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

## 1.1.3  -- 2018-07-30

### Changed
- Fixed a major bug with persistent homology; high dimensional holes were being detected in low dimensional data sets.
- Persistent homology now filters out bar codes of the form (i, Just i), as they say nothing about the topology of the underlying complexes.