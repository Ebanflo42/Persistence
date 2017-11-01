module Matrix where

import Util

data Matrix a = Matrix [[a]] a

getElems (Matrix elems _) = elems
getOrder (Matrix _ order) = order