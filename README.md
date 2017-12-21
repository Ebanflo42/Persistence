# Persistence
A topological data analysis library for Haskell.

Relevant files: Util.hs, Matrix.hs, SimplicialComplex.hs

Major TODOs:
1) Implement parallelism for finding the kernel of an integer matrix
2) Test the construction of the Vietoris-Rips Complex
3) Implement persistence modules.

Papers for learning about topological data analysis:

https://pdfs.semanticscholar.org/e503/c24dcc7a8110a001ae653913ccd064c1044b.pdf

http://geometry.stanford.edu/papers/zc-cph-05/zc-cph-05.pdf

Overview of SimplicialComplex

Simplicial complexes are represented as linked lists of vectors of pairs of integer vectors.
Each index of the list represents all simplices of that dimension.
A simplex is represented by a pair - a vector with its veritces and a vector with the indices
of its faces in the next lowest index of the list.

This module provides functions for constructing the Vietoris-Rips complex and calculating homology
over both the integers and the integers modulo 2 (represented with booleans).
The construction of the Vietoris-Rips complex has two steps when starting from a metric data set:
1) Construct the neighborhood graph, simply make an edge between any two points that fall within the given distance
2) Construct the clique complex of the neighborhood graph. This is far more complicated and the current algorithm is untested

Homology groups are represented by integer lists. An element being 0 in the list represents a factor
of the infinite cyclic group in the homology group. An element k /= 0 represents a factor of the
cyclic group of order k in the homology group. So an element of 1 represents a factor of the trivial group, i.e. no factor.
The nth homology group is the quotient of the kernel of the nth boundary operator by the image of the (n+1)th boundary operator.
First, the kernel of the nth boundary operator is found (in Matrix) and its basis is arranged into the rows of a matrix.
Since the image of the (n+1)th boundary operator is its column space, it is left-multiplied by the kernel matrix
to project the basis of the image onto the basis of the kernel, a change of coordinates. Once this is done,
the Smith normal form of that matrix is computed so that we can see how the basis of one vector space fits into the other.
The diagonal of the Smith normal form represents the nth homology group.
