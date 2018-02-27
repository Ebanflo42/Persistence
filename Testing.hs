import Util
import Matrix
import SimplicialComplex
import Persistence
import Data.Vector as V
import Data.List as L

matrix1 :: IMatrix
matrix1 =
  cons (cons 2 $ cons 3 $ cons 5 empty)
    (cons (cons (-4) $ cons 2 $ cons 3 empty) empty)

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
    [ [1, 0, (-3), 0, 2, (-8)]
    , [0, 1, 5, 0, (-1), 4]
    , [0, 0, 0, 1, 7, (-9)]
    , [0, 0, 0, 0, 0, 0] ]

kernel3 :: IMatrix
kernel3 =
  V.fromList $ L.map V.fromList $
    [ [3,(-5),1,0,0,0]
    , [(-2),1,0,(-7),1,0]
    , [8,(-4),0,9,0,1] ]

matrix4 :: IMatrix
matrix4 =
  cons (cons 2 $ cons 4 $ cons 4 empty) $
    cons (cons (-6) $ cons 6 $ cons 12 empty) $
      cons (cons 10 $ cons (-4) $ cons (-16) empty) empty

snf4 :: IMatrix
snf4 = 
  cons (cons 2 $ cons 0 $ cons 0 empty) $
    cons (cons 0 $ cons 6 $ cons 0 empty) $
      cons (cons 0 $ cons 0 $ cons 12 empty) empty

matrix5 :: IMatrix
matrix5 =
  V.fromList $ L.map V.fromList $
    [ [9, -36, 30]
    , [-36, 192, -180]
    , [30, -180, 180] ]

snf5 :: IMatrix
snf5 =
  V.fromList $ L.map V.fromList $
    [ [3, 0, 0]
    , [0, 12, 0]
    , [0, 0, 60] ]

matrix6 :: IMatrix
matrix6 =
  V.fromList $ L.map V.fromList $
    [ [9,   0, -36,  30]
    , [0,   0,   0,   0]
    , [-36, 0,  19, -18]
    , [30,  0, -18, 18] ]


printMat :: IMatrix -> String
printMat mat  =
  let printVec vec =
        if V.null vec then ""
        else
          let x = V.head vec
          in (show x) L.++ ((if x < 0 then " " else "  ") L.++ (printVec $ V.tail vec))
      print m =
        if V.null m then ""
        else (printVec $ V.head m) L.++ ('\n':(print $ V.tail m))
  in print mat

printMatBool :: BMatrix -> String
printMatBool mat  =
  let printVec vec =
        if V.null vec then ""
        else (if V.head vec then "1 " else "0 ") L.++ (printVec $ V.tail vec)
      print m =
        if V.null m then ""
        else (printVec $ V.head m) L.++ ('\n':(print $ V.tail m))
  in print mat

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

testVR = makeVRComplex 10.0 metric pointCloud

boundOps = makeBoundaryOperatorsInt testVR

boolOps = makeBoundaryOperatorsBool testVR

main = do

  putStrLn "The first matrix is:"
  putStrLn $ printMat matrix1
  putStrLn "It's kernel is:"
  putStrLn $ printMat $ kernelInt matrix1
  putStrLn "It's kernel computed in parallel is:"
  putStrLn $ printMat $ kernelIntPar matrix1
  putStrLn "The kernel should be:"
  putStrLn $ printMat kernel1

  putStrLn "The second matrix is:"
  putStrLn $ printMat matrix2
  putStrLn "It's kernel is:"
  putStrLn $ printMat $ kernelInt matrix2
  putStrLn "It's kernel computed in parallel is:"
  putStrLn $ printMat $ kernelIntPar matrix2
  putStrLn "The kernel should be:"
  putStrLn $ printMat kernel2

  putStrLn "The third matrix is:"
  putStrLn $ printMat matrix3
  putStrLn "It's kernel is:"
  putStrLn $ printMat $ kernelInt matrix3
  putStrLn "It's kernel computed in parallel is:"
  putStrLn $ printMat $ kernelIntPar matrix3
  putStrLn "The kernel should be:"
  putStrLn $ printMat kernel3

  putStrLn "The fourth matrix is:"
  putStrLn $ printMat matrix4
  putStrLn "It's Smith normal form is:"
  putStrLn $ printMat $ normalFormInt matrix4
  putStrLn "It's Smith normal form computed in parallel is:"
  putStrLn $ printMat $ normalFormIntPar matrix4
  putStrLn "The Smith Normal form should be:"
  putStrLn $ printMat $ snf4

  putStrLn "The fifth matrix is:"
  putStrLn $ printMat matrix5
  putStrLn "It's Smith normal form is:"
  putStrLn $ printMat $ normalFormInt matrix5
  putStrLn "It's Smith normal form computed in parallel is:"
  putStrLn $ printMat $ normalFormIntPar matrix5
  putStrLn "The Smith Normal form should be:"
  putStrLn $ printMat $ snf5

  putStrLn "The sixthth matrix is:"
  putStrLn $ printMat matrix6
  putStrLn "It's Smith normal form is:"
  putStrLn $ printMat $ normalFormInt matrix6
  putStrLn "It's Smith normal form computed in parallel is:"
  putStrLn $ printMat $ normalFormIntPar matrix6

  putStrLn "The Vietoris-Rips complex, scale 10.0, is:"
  putStrLn $ sc2String testVR
  {--}
  putStrLn "The boundary operators are:"
  let strMat = V.toList $ V.map printMat boundOps
  putStrLn $ intercalate "\n" $ strMat
  --}
  putStrLn "The homology groups are:"
  putStrLn $ intercalate "\n" $ L.map show $ calculateHomologyIntPar testVR
{--}
  putStrLn "Boundary operator 0 times boundary operator 1:"
  putStrLn $ printMat $ (boundOps ! 0) `multiply` (boundOps ! 1)

  putStrLn "Boundary operator 1 times boundary operator 2:"
  putStrLn $ printMat $ (boundOps ! 1) `multiply` (boundOps ! 2)
--}
{--}
  putStrLn "Ranks of the boolean homology groups:"
  putStrLn $ intercalate "\n" $ L.map show $ calculateHomologyBool testVR
  putStrLn "Boolean boundary operators:"
  putStrLn $ intercalate "\n" $ V.toList $ V.map printMatBool boolOps 
{--}
  putStrLn "Boundary operator 0 times boundary operator 1:"
  putStrLn $ printMatBool $ (boolOps ! 0) `multiply` (boolOps ! 1)

  putStrLn "Boundary operator 1 times boundary operator 2:"
  putStrLn $ printMatBool $ (boolOps ! 1) `multiply` (boolOps ! 2)
  --}
{--
  let filtration = makeFiltration [10.0] metric pointCloud
  putStrLn "The filtration is:"
  putStrLn $ filtr2String filtration
  --}