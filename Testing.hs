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
  V.fromList $ L.map V.fromList $
    [[1, 0, (-3), 0, 2, (-8)], [0, 1, 5, 0, (-1), 4], [0, 0, 0, 1, 7, (-9)], [0, 0, 0, 0, 0, 0]]

kernel3 :: IMatrix
kernel3 =
  V.fromList $ L.map V.fromList $
  [[3,(-5),1,0,0,0], [(-2),1,0,(-7),1,0], [8,(-4),0,9,0,1]]

matrix4 :: IMatrix
matrix4 = cons (cons 2 $ cons 4 $ cons 4 empty) $ cons (cons (-6) $ cons 6 $ cons 12 empty) $ cons (cons 10 $ cons (-4) $ cons (-16) empty) empty

snf4 :: IMatrix
snf4 = 
  cons (cons 2 $ cons 0 $ cons 0 empty) $
    cons (cons 0 $ cons 6 $ cons 0 empty) $
      cons (cons 0 $ cons 0 $ cons 12 empty) empty

matrix5 :: IMatrix
matrix5 =
  V.fromList $ L.map V.fromList $
    [[9, -36, 30], [-36, 192, -180], [30, -180, 180]]

snf5 :: IMatrix
snf5 =
  V.fromList $ L.map V.fromList $
    [[3, 0, 0], [0, 12, 0], [0, 0, 60]]

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

main = do

  putStrLn "The first matrix is:"
  putStrLn $ mat2String matrix1
  putStrLn "It's kernel is:"
  putStrLn $ mat2String $ findKernelInt matrix1
  putStrLn "It's kernel computed in parallel is:"
  putStrLn $ mat2String $ findKernelIntPar matrix1
  putStrLn "The kernel should be:"
  putStrLn $ mat2String kernel1

  putStrLn "The second matrix is:"
  putStrLn $ mat2String matrix2
  putStrLn "It's kernel is:"
  putStrLn $ mat2String $ findKernelInt matrix2
  putStrLn "It's kernel computed in parallel is:"
  putStrLn $ mat2String $ findKernelIntPar matrix2
  putStrLn "The kernel should be:"
  putStrLn $ mat2String kernel2

  putStrLn "The third matrix is:"
  putStrLn $ mat2String matrix3
  putStrLn "It's kernel is:"
  putStrLn $ mat2String $ findKernelInt matrix3
  putStrLn "It's kernel computed in parallel is:"
  putStrLn $ mat2String $ findKernelIntPar matrix3
  putStrLn "The kernel should be:"
  putStrLn $ mat2String kernel3

  putStrLn "The fourth matrix is:"
  putStrLn $ mat2String matrix4
  putStrLn "It's Smith normal form is:"
  putStrLn $ mat2String $ getSmithNormalFormInt matrix4
  putStrLn "It's Smith normal form computed in parallel is:"
  putStrLn $ mat2String $ getSmithNormalFormIntPar matrix4
  putStrLn "The Smith Normal form should be:"
  putStrLn $ mat2String $ snf4

  putStrLn "The fifth matrix is:"
  putStrLn $ mat2String matrix5
  putStrLn "It's Smith normal form is:"
  putStrLn $ mat2String $ getSmithNormalFormInt matrix5
  putStrLn "It's Smith normal form computed in parallel is:"
  putStrLn $ mat2String $ getSmithNormalFormIntPar matrix5
  putStrLn "The Smith Normal form should be:"
  putStrLn $ mat2String $ snf5

  let testVR = makeVRComplex 10.0 metric pointCloud
  putStrLn "The Vietoris-Rips complex, scale 10.0, is:"
  putStrLn $ sc2String testVR
  putStrLn "The homology groups are:"
  putStrLn $ intercalate "\n" $ L.map show $ calculateHomologyIntPar testVR
  putStrLn "The boundary operators are:"
  let boundOps = makeBoundaryOperatorsInt testVR; strMat = V.toList $ V.map V.toList $ V.map (V.map show) boundOps
  putStrLn $ intercalate "\n" $ L.map (intercalate "\n") $ strMat