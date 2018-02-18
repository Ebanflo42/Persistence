# Persistence
A topological data analysis library for Haskell.

Relevant files: Util.hs, Matrix.hs, SimplicialComplex.hs, Persistence.hs, Testing.hs

Compile Testing.hs with `ghc --make Testing.hs -threaded -rtsopts` and run with 

    ./Testing +RTS -s -N<number of threads>

The objective of the library is to provide users with functions for computing simplicial and persistent homology. These two are very related to each other, but the computation is very different.

Major TODOs:

My objective is to get simplicial homology over the integers and integers modulo 2 working before I move on to persistent homology.
There are a few things that could be causing the malfunctioning of simplicial homology:
1) The most likely is that fact that, while the nth and (n+1)th boundary operators are being transformed to find the image of one in the basis of the kernel of the other, their product is not always the zero matrix. This is something I noticed in the result of imgInKerInt but I haven't been able to pinpoint what's causing it.
2) For some reason, one of the test cases for computing the Smith normal form of an integer matrix has a sign flipped in one of the diagonal entries. I'm not sure why this is or whether or not it will affect the homology groups.
Known unaccounted for edge-cases:
1) If the algorithm for computing the Smith normal form of an integer matrix runs into a zero column and zero row which intersect each other along the diagonal, it will ignore this and move on, leaving a zero in the middle of the diagonal which shouldn't be there (unless all entries after it are zero) because it will never be able to divide the next diagonal entry.
General:
1) A more consistent, well-motivated, and concise philosophy for parallelism needs to be implemented; current tests reveal that many sparks are fizzling or being garbage-collected.

A good blog post on computing simplicial homology:

https://jeremykun.com/2013/04/10/computing-homology/

Papers for learning about topological data analysis:

https://pdfs.semanticscholar.org/e503/c24dcc7a8110a001ae653913ccd064c1044b.pdf

http://geometry.stanford.edu/papers/zc-cph-05/zc-cph-05.pdf

Overview of SimplicialComplex

Simplicial complexes are represented as a pair. The first component is an integer
indicating the number of vertices (might remove that) and the second is a list
of arrays of simplices whose dimension is given by the index in the list +2.

This module provides functions for constructing the Vietoris-Rips complex and calculating homology
over both the integers and the integers modulo 2 (represented with booleans).

The Vietoris-Rips complex is constructed by first finding all maximal cliques of the data set given
the metric and scale (all arrras of points which fall within the scale of each other) and then
enumerating all the faces of the cliques.

Homology groups are represented by integer lists. An element being 0 in the list represents a factor
of the infinite cyclic group in the homology group. An element k /= 0 represents a factor of the
cyclic group of order k in the homology group. So an element of 1 represents a factor of the trivial group, i.e. no factor.

The nth homology group is the quotient of the kernel of the nth boundary operator by the image of the (n+1)th boundary operator.
First, the kernel of the nth boundary operator is found (in Matrix.hs) and its basis is arranged into the rows of a matrix.
Since the image of the (n+1)th boundary operator is its column space, it is left-multiplied by the kernel matrix
to project the basis of the image onto the basis of the kernel, a change of coordinates. Once this is done,
the Smith normal form of that matrix is computed so that we can see how the basis of one vector space fits into the other.
The diagonal of the Smith normal form represents the nth homology group.

Overview of Matrix

Matrices are transformed by iterating through each row and selecting a pivot
The pivot is the diagonal entry of the row, and must be non-zero
If the diagonal entry is non-zero at first, a switch is performed so that it is

To get the smith normal form, the entire pivot row and column is eliminated before continuing

To get the kernel, the matrix is first put into column eschelon form. To get column eschelon form,
every element in the pivot row after the pivot is eliminated. All column operations to get the
matrix to this form are also performed on the identiy matrix. This is why many of the functions
for getting the kernel of a matrix take two matrices, one is the image of the identity under all
the column operations and the other is the matrix being reduced. Once this is over, the columns of
the former identity matrix corresponding to the zero columns of the column-eschelon form are the
basis of the kernel, the basis vectors are then made into the rows of a matrix

Eliminating elements is a slighltly more complicated process since only integer operations are allowed.
First, every element that must be eliminated is made divisible by the pivt using the bezout coefficients
from the extended Euclidean algorithm. Once this is done, integer division and subtraction can be used
to eliminate the elements.

Boolean matrices are regular matrices with elements modulo 2, Bool is an instance
of Num here and the instance is given in Util.
