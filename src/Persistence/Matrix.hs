{- |
Module     : Persistence.Matrix
Copyright  : (c) Eben Kadile, 2018
License    : BSD 3 Clause
Maintainer : eben.cowley42@gmail.com
Stability  : experimental

This module contains a variety of matrix utility functions, used in the computation of Betti numbers and simplicial homology groups.

Most importantly, it includes functions for computing the rank, normal form, and kernel of matrices. For the computation of homology groups and Betti numbers, one must perform column operations on one matrix to get it into column echelon form and find its kernel while also performing the inverse row operations on the next matrix to be operated on.

Bool is an instance of Num here (instance given in Util) so that functions can be somewhat generalized to act on both integers and integers modulo 2.

-}

module Persistence.Matrix where

import Persistence.Util

import Data.Word
import Data.List           as L
import Data.Vector         as V
import Data.Vector.Unboxed as UV

import Control.Parallel.Strategies

-- * Types

{- |
  Boolean vectors are represented by unboxed vectors of 64-bit values.
  The integer represents the dimension of the vector, i.e. how many bits are actually relevant to the result of computations.
-}
data BVector = BVector Int (UV.Vector Word64)

-- | A boxed vector whose elements are the rows of the matrix, as well as a flag determining whether or not the matrix has been transposed.
data BMatrix = BMatrix Bool (V.Vector BVector)