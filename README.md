# Persistence
A topological data analysis library for Haskell.

There are two functions for finding smith normal form - one is working but has to repeatedly transpose the matrix that it works on.
The other doesn't do this but currently isn't working.

Parallelism needs to be improved (need to switch to Control.Monad.Par instead of Control.Parallel)

The construction of the VR complex has not been tested and neither has the function for finding the kernel of a matrix.
