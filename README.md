# Persistence
A topological data analysis library for Haskell.

Relevant files: Util.hs, Matrix.hs, SimplicialComplex.hs, Persistence.hs, Testing.hs

Compile Testing.hs with `ghc --make Testing.hs -threaded -rtsopts` and run with 

    ./Testing +RTS -s -N<number of threads>

The objective of the library is to provide users with functions for computing simplicial and persistent homology. These two are very related to each other, but the computation is very different.

Major TODOs:

Matrix:

1) There is an edge case with the Smith normal form of a matrix where a zero row intersects a zero column on the diagonal. This needs to be fixed.

2) Implement integer polynomial functions.

Simplicial Complex:

1) Continue debugging homology over the integers (probably requires fixing Smith normal form).

Persistence:

1) Ensure that construction of the boundary operators is complete for F_2 and implement persistent homology over F_2.

2) Investigate the second paper further to ensure a good understanding of how to calculate persistent homology over PID's then implement persistent homology over the integers.

General:
1) A more consistent, well-motivated, and concise philosophy for parallelism needs to be implemented; current tests reveal that many sparks are fizzling or being garbage-collected.

A good blog post on computing simplicial homology:

https://jeremykun.com/2013/04/10/computing-homology/

Papers for learning about topological data analysis:

https://pdfs.semanticscholar.org/e503/c24dcc7a8110a001ae653913ccd064c1044b.pdf

http://geometry.stanford.edu/papers/zc-cph-05/zc-cph-05.pdf

Overview of SimplicialComplex

Simplicial complexes are represented as a pair. The first component is an integer indicating the number of vertices and the second is a list of arrays of simplices whose dimension is given by the index in the outer list +2.

This module provides functions for constructing the Vietoris-Rips complex and calculating homology over both the integers and the integers modulo 2 (represented with booleans).

The Vietoris-Rips complex is constructed by first finding all maximal cliques of the data set given the metric and scale (all arrrays of points which fall within the scale of each other) and then enumerating all the faces of the cliques.

Integer homology groups are represented by integer lists. An element being 0 in the list represents a factor of the infinite cyclic group in the homology group. An element k /= 0 represents a factor of the cyclic group of order k in the homology group. So an element of 1 represents a factor of the trivial group, i.e. no factor.

The nth homology group is the quotient of the kernel of the nth boundary operator by the image of the (n+1)th boundary operator. The boundary operators represented by rectangular 2D arrays.

For homology over the integers, one must first put the nth boundary operator in column eschelon form and perform the corresponding inverse row operations on the n+1th boundary operator. After this process is complete the column space of the rows of the n+1th corresponding to zero columns in the column eschelon form is the image of the n+1th represented in the basis of the kernel of the nth. See the second paper. These are the two modules we need to quotient; to get the representation of the quotient as a direct product of cyclic groups we look at the diagonal of the Smith normal form of the afformentioned matrix.

Simplicial homology over F2 is much simpler. The only information we could possibly need from any homology group is its rank as an F_2 vector space. Since it is a quotient space, this is simply the number of n-simplices in the complex minus the rank of the nth boundary operator minus the rank of the n+1th boundary operator.

Overview of Matrix

Matrices are transformed by iterating through each row and selecting a pivot. Zero rows are skipped for finding column eschelon form but a row operation is performed (if possible) if there is a zero row for Smith normal form.

To get the smith normal form, the entire pivot row and column is eliminated before continuing

To get column eschelon form, every element in the pivot row after the pivot is eliminated. To get the kernel, all column operations to get the matrix to this form are also performed on the identiy matrix. To get the image of one matrix inside the kernel of the one being put into column eschelon form, perform the inverse operations on the matrix whose image is needed. See second paper.

To get the rank of a matrix, look at the number of non-zero columns in the column eschelon form. To get the kernel, look at the columns of the identity (after all of the same column operations have been performed on it) which correspond to zero columns of the column eschelon form.

Eliminating elements is a slighltly more complicated process since only integer operations are allowed. First, every element that must be eliminated is made divisible by the pivt using the bezout coefficients from the extended Euclidean algorithm. Once this is done, integer division and subtraction can be used to eliminate the elements.

Boolean matrices are regular matrices with elements modulo 2, Bool is an instance of Num here and the instance is given in Util.
