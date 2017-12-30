# Persistence
A topological data analysis library for Haskell.

Relevant files: Util.hs, Matrix.hs, SimplicialComplex.hs, Testing.hs

Compile Testing.hs with `ghc --make Testing.hs -threaded -rtsopts` and run with 

    ./Testing +RTS -s -N<number of threads>

Major TODOs:
1) Test the construction of the Vietoris-Rips Complex
2) Implement persistence modules.

Papers for learning about topological data analysis:

https://pdfs.semanticscholar.org/e503/c24dcc7a8110a001ae653913ccd064c1044b.pdf

http://geometry.stanford.edu/papers/zc-cph-05/zc-cph-05.pdf

Overview of SimplicialComplex

Simplicial complexes are represented by linked lists of arrays of pairs of integer arrays.
Each index of the list represents all simplices of that dimension.
A simplex is represented by a pair - an array with its veritces and a vector with the indices
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
