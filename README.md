# Persistence
A topological data analysis library for Haskell.

Relevant files: Util.hs, Matrix.hs, SimplicialComplex.hs, Persistence.hs, Testing.hs

Compile Testing.hs with `ghc --make Testing.hs -threaded -rtsopts` and run with 

    ./Testing +RTS -s -N<number of threads>

The objective of the library is to provide users with functions for computing simplicial and persistent homology. These two are very related to each other, but the computation is very different.

Major TODOs:

`Matrix.hs`:

1) Fix Gauss-Jordan elimnation over the integers.

`SimplicialComplex.hs`:

1) Implement construction of the clique complex for graphs.

2) Implement construction of the directed clique complex for directed graphs (simplices are acyclic complete subgraphs instead of just complete subgraphs).

3) Fix simplicial homology over the integers (this is almost certainly being caused by a malfunction of Gauss-Jordan elimnation in `Matrix.hs`

`Persistence.hs`:

1) Fix infinite barcodes.

2) Implement filtration by weights for the clique complex of a weighted graph.

3) Implement filtration by weights for the directed clique complex of a directed weighted graph.

`Testing.hs`:

1) Document the expected homology outputs for each of the point clouds.

General:

1) A more consistent, well-motivated, and concise philosophy for parallelism needs to be implemented.

# Learning about Topological Data Analysis

Simplicial homology:

https://jeremykun.com/2013/04/10/computing-homology/

The Vietoris-Rips complex:

https://pdfs.semanticscholar.org/e503/c24dcc7a8110a001ae653913ccd064c1044b.pdf

Persistent homology:

http://geometry.stanford.edu/papers/zc-cph-05/zc-cph-05.pdf

# Overview of SimplicialComplex

Simplicial complexes are represented as a pair. The first component is an integer indicating the number of vertices and the second is a list of arrays of simplices whose dimension is given by the index in the outer list +2.

This module provides functions for constructing the Vietoris-Rips complex and calculating homology over both the integers and the integers modulo 2 (represented with booleans).

The Vietoris-Rips complex is constructed by first finding all maximal cliques of the data set given the metric and scale (all arrrays of points which fall within the scale of each other) and then enumerating all the faces of the cliques.

An important aspect of this module, as well as the main Persistence.hs file, is the existence of "light" vs. "fast" functions. Since the distance between data points needs to be repeatedly evaluated for the construction of the Vietoris-Rips complex, "fast" functions construct a square array of distances between every single point in the data so that the distance can be retrieved extremely quickly. Since the memory cost of this array is n^2 wrt the number of data points, this could require a tremendous amount of memory for a substantially large data set - this is why "light" functions don't use this optimization.

Integer homology groups are represented by integer lists. An element being 0 in the list represents a factor of the infinite cyclic group in the homology group. An element k /= 0 represents a factor of the cyclic group of order k in the homology group. So an element of 1 represents a factor of the trivial group, i.e. no factor.

The nth homology group is the quotient of the kernel of the nth boundary operator by the image of the (n+1)th boundary operator. The boundary operators represented by rectangular 2D arrays.

For homology over the integers, one must first put the nth boundary operator in column eschelon form and perform the corresponding inverse row operations on the n+1th boundary operator. After this process is complete the column space of the rows of the n+1th corresponding to zero columns in the column eschelon form is the image of the n+1th represented in the basis of the kernel of the nth (See the Stanford paper). These are the two modules we need to quotient; to get the representation of the quotient as a direct product of cyclic groups we look at the diagonal of the Smith normal form of the afformentioned matrix.

Simplicial homology over F_2 (the field with 2 elements) is much simpler. The only information we could possibly need from any homology group is its dimension as an F_2 vector space. Since it is a quotient space, this is simply the number of n-simplices in the complex minus the rank of the nth boundary operator minus the rank of the n+1th boundary operator.


# Overview of Matrix

Matrices are transformed by iterating through each row and selecting a pivot. Zero rows are skipped for finding column eschelon form but a row operation is performed (if possible) if there is a zero row for Smith normal form.

To get the smith normal form, the entire pivot row and column is eliminated before continuing. Also, the pivot is always a diagonal element.

To get column eschelon form, every element in the pivot row after the pivot is eliminated. To get the kernel, all column operations to get the matrix to this form are also performed on the identiy matrix. To get the image of one matrix inside the kernel of the one being put into column eschelon form, perform the inverse row operations on the matrix whose image is needed. See stanford paper or the blog post on simplicial homology.

To get the rank of a matrix, look at the number of non-zero columns in the column eschelon form. To get the kernel, look at the columns of the identity (after all of the same column operations have been performed on it) which correspond to zero columns of the column eschelon form.

Eliminating elements is a slighltly more complicated process since only integer operations are allowed. First, every element that must be eliminated is made divisible by the pivot by using the Bezout coefficients from the extended Euclidean algorithm. Once this is done, integer division and subtraction can be used to eliminate the elements.

Boolean matrices are much easier to work with, they are regular matrices with elements modulo 2. Bool is an instance of Num here and the instance is given in Util.

Matrices whose elements are monomials with Boolean coefficients are also operated on. According to the Stanford paper, these aren't really necessary but it really helps keep track of all the boundary chains. Finding the column echelon form of these matrices is almost exactly like finding it for ordinary matrices with coefficients from F_2 except one has to make sure that the monomial with the least degree is in the pivot position (otherwise you will end up dividing a monomial by a monomial with a larger degree).
