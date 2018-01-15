import Util
import Matrix
import SimplicialComplex
import Data.Vector as V
import Data.List as L

matrix1 :: IMatrix
matrix1 = cons (cons 2 $ cons 3 $ cons 5 empty) (cons (cons (-4) $ cons 2 $ cons 3 empty) empty )

kernel1 :: IMatrix
kernel1 = cons ( cons (-1) $ cons (-26) $ cons 16 empty) empty

matrix2 :: IMatrix
matrix2 =
  cons (cons 1 $ cons (-1) $ cons 3 empty) $
    cons (cons 5 $ cons 6 $ cons (-4) empty) $
      cons (cons 7 $ cons 4 $ cons 2 empty) empty

kernel2 :: IMatrix
kernel2 = cons (cons (-14) $ cons 19 $ cons 11 empty) empty

matrix3 :: IMatrix
matrix3 =
  cons (cons 1 $ cons 0 $ cons 1 empty) $
    cons (cons 2 $ cons (-1) $ cons 1 empty) empty

kernel3 :: IMatrix
kernel3 = cons (cons 1 $ cons 1 $ cons (-1) empty) empty

matrix4 :: IMatrix
matrix4 = cons (cons 2 $ cons 4 $ cons 4 empty) $ cons (cons (-6) $ cons 6 $ cons 12 empty) $ cons (cons 10 $ cons (-4) $ cons (-16) empty) empty

snf :: IMatrix
snf = 
  cons (cons 2 $ cons 0 $ cons 0 empty) $
    cons (cons (-6) $ cons 6 $ cons 12 empty) $
      cons (cons 10 $ cons (-4) $ cons (-16) empty) empty

mat2String :: IMatrix -> String
mat2String matrix =
  let helper mat =
        if V.null mat then ""
        else let x = V.head mat; xs = V.tail mat in
          (show x) Prelude.++ ('\n':(helper xs)) in
  helper matrix

pointCloud :: [(Float, Float)]
pointCloud =
  [ ( -7, -19)
  , ( -6, -16)
  , (  2, -16)
  , ( 11, -16)
  , ( 11, -11)
  , ( -6,  -9)
  , (  2,  -9)
  , ( 11,  -6)
  , (-11,  -5)
  , (  6,  -3)
  , (  0,   0)
  , ( -2,   3)
  , ( 11,   3)
  , (-14,   8)
  , (  1,   8)
  , ( -8,  14)
  , ( 10,  19)
  , ( 17,  20)
  ]

metric :: (Float, Float) -> (Float, Float) -> Float
metric (a, b) (c, d) =
  let x = a - c; y = b - d in
  sqrt (x * x + y * y)

vr10 :: SimplicialComplex
vr10 =
  (18,
    [
      V.fromList $
        L.map V.fromList $
          [
            [16,17],
            [13,15],
            [2,6],
            [5,8],
            [5,6],
            [1,5],
            [3,4],
            [2,3],
            [11,14],
            [10,14],
            [10,11],
            [9,12],
            [7,12],
            [9,10],
            [6,10],
            [1,2],
            [0,2],
            [0,1],
            [7,9],
            [6,9],
            [4,9],
            [6,7],
            [4,7],
            [4,6]
          ],
    V.fromList $
      L.map V.fromList $
        [
          [10,11,14],
          [7,9,12],
          [6,9,10],
          [0,1,2],
          [6,7,9],
          [4,7,9],
          [4,6,9],
          [4,6,7]
        ],
    cons (V.fromList [4,6,7,9]) empty
    ]
  )

main = do

  putStrLn "The first matrix is:"
  putStrLn $ mat2String matrix1
  putStrLn "It's kernel is:"
  putStrLn $ mat2String $ findKernelInt matrix1
  putStrLn "It's kernel computed in parallel is:"
  putStrLn $ mat2String $ findKernelIntPar matrix1
  putStrLn "The kernel should be:"
  putStrLn $ mat2String kernel1
  putStrLn "It's Smith normal form is:"
  putStrLn $ mat2String $ getSmithNormalFormInt matrix1
  putStrLn "Its Smith normal form computed in parallel is:"
  putStrLn $ mat2String $ getSmithNormalFormIntPar matrix1

  putStrLn "The second matrix is:"
  putStrLn $ mat2String matrix2
  putStrLn "It's kernel is:"
  putStrLn $ mat2String $ findKernelInt matrix2
  putStrLn "It's kernel computed in parallel is:"
  putStrLn $ mat2String $ findKernelIntPar matrix2
  putStrLn "The kernel should be:"
  putStrLn $ mat2String kernel2
  putStrLn "It's Smith normal form is:"
  putStrLn $ mat2String $ getSmithNormalFormInt matrix2
  putStrLn "Its Smith normal form computed in parallel is:"
  putStrLn $ mat2String $ getSmithNormalFormIntPar matrix2

  putStrLn "The third matrix is:"
  putStrLn $ mat2String matrix3
  putStrLn "It's kernel is:"
  putStrLn $ mat2String $ findKernelInt matrix3
  putStrLn "It's kernel computed in parallel is:"
  putStrLn $ mat2String $ findKernelIntPar matrix3
  putStrLn "The kernel should be:"
  putStrLn $ mat2String kernel3
  putStrLn "It's Smith normal form is:"
  putStrLn $ mat2String $ getSmithNormalFormInt matrix3
  putStrLn "Its Smith normal form computed in parallel is:"
  putStrLn $ mat2String $ getSmithNormalFormIntPar matrix3

  putStrLn "The matrix for Smith normal form is:"
  putStrLn $ mat2String matrix4
  putStrLn "It's Smith normal form is:"
  putStrLn $ mat2String $ getSmithNormalFormInt matrix4
  putStrLn "It's Smith normal form computed in parallel is:"
  putStrLn $ mat2String $ getSmithNormalFormIntPar matrix4

  putStrLn "The Vietoris-Rips complex, scale 10.0, is:"
  putStrLn $ sc2String $ makeVRComplex 10.0 metric pointCloud

  putStrLn "The Vietoris-Rips complex, scale 10.0, should be:"
  putStrLn $ sc2String $ vr10