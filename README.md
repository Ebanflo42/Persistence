# Persistence
A topological data analysis library for Haskell.

This library is motivated by flexibility when it comes to the type of data being analyzed. If your data comes with a meaningful binary function into into an ordered set, you can use Persistence to analyze your data. The library also provides functions for analyzing directed\/undirected, weighted\/unweighted graphs.

Visit https://hackage.haskell.org/package/Persistence to see the documentation for the stable version. There is also documentation in each module.

GitHub: https://github.com/Ebanflo42/Persistence

If you have the Haskell `stack` tool installed, compile and run tests with `stack test` (you may have to expose the modules `Util` and `Matrix` in the file `Persistence.cabal` to get it to work).

# Update March 16 2021

I do not plan on further developing this package; my research has taken me in directions which are rather far-removed from TDA.

If you came across this library because you are interested in TDA, check out the resources below.

If you came across this library because you are interested in a performant implementation of TDA in Haskell, this library will probably be dissatisfactory. Persistence 2 *works*, but it is written very naively using boxed vectors and often nested boxed vectors which get copied and rearranged an excessive amount during runtime. This library may be useful as a reference for the fundamental logic of a pure-functional implementation of algorithms in TDA, but in order for reasonable performance to be achieved it must be re-written using unboxed, probably mutable, arrays. I currently have no plans to perform this re-writing.

# Learning about Topological Data Analysis

Computing simplicial homology:

https://jeremykun.com/2013/04/10/computing-homology/

Constructing the Vietoris-Rips complex:

https://pdfs.semanticscholar.org/e503/c24dcc7a8110a001ae653913ccd064c1044b.pdf

Constructing the Cech complex:

https://www.academia.edu/15228439/Efficient_construction_of_the_%C4%8Cech_complex

Computing persistent homology:

http://geometry.stanford.edu/papers/zc-cph-05/zc-cph-05.pdf

The algorithm for finding the directed clique complex is inspired by the pseudocode in the supplementary materials of this paper:

https://www.frontiersin.org/articles/10.3389/fncom.2017.00048/full

Computing and working with persistence landscapes:

https://academic.csuohio.edu/bubenik_p/papers/persistenceLandscapes.pdf

# Major TODOs:

`Testing.hs`:

1) More tests for Persistence landscape functions.

2) Make some filtrations whose vertices don't all have index 0 and test persistent homology on them.

`Filtration.hs`

1) `indexBarCodes` and `indexBarCodesSimple` are both broken from the transition to unboxed vectors.

`SimplicialComplex.hs`:

1) Fix simplicial homology over the integers.

2) Implement construction of the Cech complex (n points form an (n-1)-simplex if balls of a certain radius centered at each of the points intersect).

3) Implement construction of the alpha-complex (sub-complex of the Delaunay triangulation where the vertices of every simplex are within a certain distance).

`Matrix.hs`:

1) This module ought to be completely rewritten for the sake of performance.

See each of the files for an overview of its inner-workings.
