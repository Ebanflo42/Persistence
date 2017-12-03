# Persistence
A topological data analysis library for Haskell.

Currently working on Gaussian elimination in Matrix.findKernel,
the function is intended to return matrix whose rows are a basis for the kernel of the input,
but it currently needs to be able to put a matrix into column eschelon form.

There are two functions for finding smith normal form - one is working but has to repeatedly transpose the matrix that it works on.
The other doesn't do this but currently isn't working.

The construction of the VR complex has not been tested.
