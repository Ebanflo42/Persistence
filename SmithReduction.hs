{-

QUIPPER LICENSE

Contributors are listed here, in alphabetical order by last name.
Unless otherwise noted, the copyright for his or her contributions
rests with each individual author. For contributions by authors whose
name is marked (ACS), the copyright rests with Applied Communication
Sciences.

Copyright (C) 2011-2016. All rights reserved.
Copyright (C) 2012-2013 Applied Communication Sciences. All rights
reserved.

Richard Eisenberg
Alexander S. Green
Peter LeFanu Lumsdaine
Keith Kim (ACS)
Siun-Chuon Mau (ACS)
Baranidharan Mohan
Won Ng (ACS)
Joel Ravelomanantsoa-Ratsimihah
Neil J. Ross
Artur Scherer (ACS)
Peter Selinger
Benoît Valiron
Alexandr Virodov (ACS)
Stephan A. Zdancewic

This research was supported by the Intelligence Advanced Research
Projects Activity (IARPA) via Department of Interior National Business
Center contract numbers D11PC20168 and D12PC00527. The U.S. Government
is authorized to reproduce and distribute reprints for Governmental
purposes notwithstanding any copyright annotation thereon. Disclaimer:
The views and conclusions contained herein are those of the authors
and should not be interpreted as necessarily representing the official
policies or endorsements, either expressed or implied, of IARPA,
DoI/NBC, or the U.S. Government.

----------------------------------------------------------------------
LICENSE

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the
  distribution.

3. The name of the authors and copyright holders may not be used to
  endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
----------------------------------------------------------------------
-}

{- |
Module     : Persistence.SmithReduction
Copyright  : See source code
License    : See source code
Author     : Peter Lumsdaine
Maintainer : eben.cowley42@gmail.com
Stability  : experimental

This module is for reducing integer matrices to their Smith normal form and is courtesy of the Quipper programing language.

-}

-- | This module provides functions for reducing (non-square) matrices
-- towards Smith Normal Form, and hence for computing the structure of
-- finitely-presented Abelian groups.
--
-- The SNF transformation is similar to Gaussian elimination, but over integer matrices,
-- (more generally, matrices over any principal ideal domain)
--
-- For background on how this is used to compute the structure of Abelian groups,
-- see the MathOverflow question <http://mathoverflow.net/questions/12009/>,
-- in particular Greg Kuperberg’s answer <http://mathoverflow.net/questions/12009#12053>.
--
-- We do not implement full SNF reduction here, but rather just as much as is
-- needed to compute the structure of finitely presented Abelian groups from
-- matrix presentations.

module SmithReduction where

import Util

import Data.Array

-- * Matrix type and basic access functions.

-- | A data type to hold an /m/×/n/ matrix (/M/[sub /ij/]),
-- with entries from an arbitrary type @a@.
--
-- The fields are: integers /m/ and /n/; a flag /t/ to indicate that a matrix
-- should be considered formally transposed; and an /m/×/n/ array /M/
-- containing the entries.  When /t/ is 'False', /m/ is the number of rows,
-- and /n/ the number of columns; when /t/ is 'True', this is reversed.
--
-- (The point of the flag is to allow efficient transposition, and hence to
-- allow operations on rows to be implemented in terms of the corresponding
-- operations on columns without loss of efficiency.)
data CLMatrix a = CLMatrix Int Int Bool (Array (Int, Int) a) deriving (Show)

-- | The transpose of a matrix
transpose :: CLMatrix a -> CLMatrix a
transpose (CLMatrix m n t mtx) = CLMatrix m n (not t) mtx

-- | The number of rows of a matrix
rows :: CLMatrix a -> Int
rows (CLMatrix m n t _) = if t then n else m

-- | The number of columns of a matrix
cols :: CLMatrix a -> Int
cols (CLMatrix m n t _) = if t then m else n

-- | The row indices of a matrix.
row_list :: CLMatrix a -> [Int]
row_list m = [0..((rows m)-1)]

-- | The column indices of a matrix.
col_list :: CLMatrix a -> [Int]
col_list m = [0..((cols m)-1)]

-- | An index tuple for a matrix, at a given row and column
idx :: CLMatrix a -> Int -> Int -> (Int, Int)
idx (CLMatrix _ _ t _) i j = if t then (j,i) else (i,j)

infix 9 !!!

-- | The matrix entry at a given row and column
(!!!) :: CLMatrix a -> (Int,Int) -> a
(!!!) mm@(CLMatrix m n t mtx) (i,j) =
    if (i >= rows mm || j >= cols mm)
        then error $ "Matrix entry lookup (!!!): bad index i=" ++ show i ++ ", j=" ++ show j
        else mtx ! idx mm i j

infixl 9 ///

-- | Update a matrix by a list of (/i/,/j/,/m_i_j/) pairs
-- (all indexes assumed in range of original matrix).
(///) :: CLMatrix a -> [(Int,Int,a)] -> CLMatrix a
(///) mm@(CLMatrix m n t mtx) l =
    CLMatrix m n t (mtx // [ (idx mm i j,e) | (i,j,e) <- l ])

-- | Construct an 'CLMatrix' from a list such as @[[1,0],[4,-5]]@.
--
-- Assumes that all inner lists are the same length,
-- and that the overall list is of length ≥1.
matrix_from_list :: [[a]] -> CLMatrix a
matrix_from_list [] = error "matrixFromList: empty list"
matrix_from_list rs@(r0:_) = CLMatrix (length rs) (length r0) False $
   array ((0,0), (length rs - 1, length r0 - 1))
   [ ((i,j),x) | (ri,i) <- zip rs [0..], (x,j) <- zip ri [0..] ]

-- | Delete a row of a matrix
delete_row :: Int -> CLMatrix a -> CLMatrix a
delete_row i0 mm@(CLMatrix m n t mtx) =
  if 0 <= i0 && i0 < rows mm
  then
    if t then CLMatrix m (n-1) t $ ixmap ((0,0),(m-1,n-2)) (\(j,i) -> (j,f i)) mtx
    else CLMatrix (m-1) n t $ ixmap ((0,0),(m-2,n-1)) (\(i,j) -> (f i,j)) mtx
  else error "delete_row: row out of range"
    where f i = if i < i0 then i else i+1

-- | Delete the first column of a matrix
delete_col :: Int -> CLMatrix a -> CLMatrix a
delete_col j0 = transpose . (delete_row j0) . transpose

-- * Smith reduction

-- | @'elim_entry_with_pivot' /M/ /i/ /j/ /j'/@: apply elementary column operations
-- to /M/ (equivalently, post-multiply by an invertible matrix) to
-- obtain /M'/ such that /M'/[sub /i/,/j/] is gcd(/M/[sub /i/,/j/], /M/[sub /i/,/j'/])
-- and /M'/[sub /i/,/j'/] is 0.
elim_entry_with_pivot :: (Integral int) => CLMatrix int -> Int -> Int -> Int -> CLMatrix int
elim_entry_with_pivot m i0 j0 j1 =
  let a = m !!! (i0,j0)
      b = m !!! (i0,j1)
  in if (a == 0 && b == 0) then m
  else
  let (d,x,y) = extEucAlg a b
      a' = a `div` d
      b' = b `div` d
  -- know that [x a + y b == d] and [d /= 0], so the matrix [[x,y],[−b',a']]
  -- is invertible; so premultiplication by it does not change the group
  -- presentation (and indeed could be obtained as a combination of elementary
  -- column operations).
  in m /// [ (i,j0, (x * m !!! (i,j0)) + (y * m !!! (i,j1))) | i <- row_list m ]
       /// [ (i,j1, (-b' * m !!! (i,j0)) + (a' * m !!! (i,j1))) | i <- row_list m]

-- | Given a matrix, repeatedly use 'elim_entry_with_pivot' to put the
-- top row into clean form (/d/,0,…,0).
clean_first_row :: (Integral int) => CLMatrix int -> CLMatrix int
clean_first_row m0 =
  foldl (\m j -> elim_entry_with_pivot m 0 0 j) m0 (tail $ col_list m0)

-- | Dual to 'clean_first_row'.
clean_first_col :: (Integral int) => CLMatrix int -> CLMatrix int
clean_first_col = transpose . clean_first_row . transpose

-- | Given a matrix, repeatedly apply 'clean_first_row' and its analogue
-- on columns until the first row and column are both in clean form.
clean_first_row_col :: (Integral int) => CLMatrix int -> CLMatrix int
clean_first_row_col m =
  if not $ all (==0) [ m !!! (0,j) | j <- tail $ col_list m ]
  then clean_first_row_col $ clean_first_row m
  else if not $ all (==0) [ m !!! (i,0) | i <- tail $ row_list m ]
  then clean_first_row_col $ clean_first_col m
  else m

-- * Structure of Abelian Groups

-- | Given a matrix, taken as presenting an Abelian group (with generators
-- corresponding to columns of the matrix, and relations specified by the
-- rows), compute the structure constants of the group, not necessarily sorted.
--
-- That is, return a list of natural numbers [/n/[sub 0],…,/n/[sub /s/]]
-- such that the group given by the input presentation is isomorphic to the
-- product of the cyclic groups ℤ\/(/n/[sub /i/]).
structure_constants_from_matrix  :: (Show int, Integral int) => CLMatrix int -> [int]
structure_constants_from_matrix m =
  if cols m == 0 then []
  else if rows m == 0 then (replicate (cols m) 0)
  else let m' = clean_first_row_col m
  in (abs $ m' !!! (0,0))
     : (structure_constants_from_matrix $ delete_row 0 $ delete_col 0 m')

-- | Given a matrix, taken as presenting an Abelian group,
-- compute the order of the group.
--
-- Returns 0 if the group is of infinite order.
group_order_from_matrix :: (Show int, Integral int) => CLMatrix int -> int
group_order_from_matrix = product . structure_constants_from_matrix