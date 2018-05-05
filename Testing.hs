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
printMat mat =
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
printMatBool mat =
  let printVec vec =
        if V.null vec then ""
        else (if V.head vec then "1 " else "0 ") L.++ (printVec $ V.tail vec)
      print m =
        if V.null m then ""
        else (printVec $ V.head m) L.++ ('\n':(print $ V.tail m))
  in print mat

pointCloud1 :: [(Float, Float)]
pointCloud1 =
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

pointCloud2 :: [(Float, Float)]
pointCloud2 =
  [ (  1, -22)
  , (  4, -21)
  , ( -3, -20)
  , (  4, -19)
  , ( -7, -18)
  , (  5, -17)
  , ( -7, -12)
  , ( -1, -12)
  , (  3, -12)
  , (  6, -12)
  , (  8, -12)
  , ( -9, -10)
  , ( -4,  -9)
  , (-12,  -8)
  , (  7,  -8)
  , ( -8,  -7)
  , ( -5,  -7)
  , (-11,  -6)
  , (  9,  -5)
  , ( -6,  -4)
  , (-14,  -3)
  , (-12,  -3)
  , (  7,  -3)
  , (  5,  -1)
  , ( -7,   0)
  , (  0,   0)
  , (  8,   0)
  , (-12,   1)
  , (  2,   1)
  , (  0,   2)
  , (-10,   3)
  , ( -7,   3)
  , (  6,   3)
  , ( -2,   5)
  , ( 10,   5)
  , ( -8,   9)
  , (  8,   9)
  , ( -9,  11)
  , (  6,  11)
  , ( -6,  12)
  , ( -4,  13)
  , (  2,  13)
  , (  8,  13)
  , ( -2,  14)
  , (  5,  14)
  , ( -4,  16)
  , (  1,  16)
  ]

octahedralCloud =
  [ ( 1, 0, 0)
  , (-1, 0, 0)
  , ( 0, 1, 0)
  , ( 0,-1, 0)
  , ( 0, 0, 1)
  , ( 0, 0,-1)
  --, (100,0,0)
  --, (100,0,1)
  --, (100,1,1)
  --, (100,1,0)
  ]

sqrCloud =
  [ (0,0)
  , (1,0)
  , (2,0)
  , (3,0)
  , (3,1)
  , (3,2)
  , (3,3)
  , (2,3)
  , (1,3)
  , (0,3)
  , (0,2)
  , (0,1)
  ]

metric2 :: (Float, Float) -> (Float, Float) -> Float
metric2 (a, b) (c, d) =
  let x = a - c; y = b - d in
  sqrt (x*x + y*y)

metric3 :: (Float, Float, Float) -> (Float, Float, Float) -> Float
metric3 (x0, y0, z0) (x1, y1, z1) =
  let dx = x1 - x0; dy = y1 - y0; dz = z1 - z0 in
  sqrt (dx*dx + dy*dy + dz*dz)

--should have 2 loops and 3 connected components
testVR = fst $ makeVRComplexFast 10.0 metric2 pointCloud1

boundOps = makeBoundaryOperatorsInt testVR

boolOps = makeBoundaryOperatorsBool testVR

--8 connected components at index 0, 2 connected components at index 1, 1 connected component at index 2
--1 loop lasting from 0 to 1, 1 loop lasting from 1 to 2, 1 loop starting at 1, 1 loop starting at 2
testFiltration = makeFiltrationFast [6.0, 5.0, 4.0] metric2 pointCloud2

--5 connected components from 0 to 2, 1 void from 2 to 3
octahedron = makeFiltrationFast [2.1, 1.6, 1.1, 0.5] metric3 octahedralCloud

--11 connected components from 0 to 1, 1 loop starting at 1
square = makeFiltrationFast [3.5, 2.0, 0.5] metric2 sqrCloud

main = do
{--
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

  putStrLn "The sixth matrix is:"
  putStrLn $ printMat matrix6
  putStrLn "It's Smith normal form is:"
  putStrLn $ printMat $ normalFormInt matrix6
  putStrLn "It's Smith normal form computed in parallel is:"
  putStrLn $ printMat $ normalFormIntPar matrix6
  --}
{--
  putStrLn "The Vietoris-Rips complex, scale 10.0, is:"
  putStrLn $ sc2String testVR
  --}
  {--
  putStrLn "The boundary operators are:"
  let strMat = V.toList $ V.map printMat boundOps
  putStrLn $ intercalate "\n" $ strMat
  --}
{--
  putStrLn "The homology groups are:"
  putStrLn $ intercalate "\n" $ L.map show $ simplicialHomologyIntPar testVR
  --}
{--
  putStrLn "Boundary operator 0 times boundary operator 1:"
  putStrLn $ printMat $ (boundOps ! 0) `multiply` (boundOps ! 1)

  putStrLn "Boundary operator 1 times boundary operator 2:"
  putStrLn $ printMat $ (boundOps ! 1) `multiply` (boundOps ! 2)
--}
{--
  putStrLn "Ranks of the boolean homology groups:"
  putStrLn $ intercalate "\n" $ L.map show $ simplicialHomologyBool testVR
  putStrLn "Boolean boundary operators:"
  putStrLn $ intercalate "\n" $ V.toList $ V.map printMatBool boolOps
  --}
{--
  putStrLn "Boundary operator 0 times boundary operator 1:"
  putStrLn $ printMatBool $ (boolOps ! 0) `multiply` (boolOps ! 1)

  putStrLn "Boundary operator 1 times boundary operator 2:"
  putStrLn $ printMatBool $ (boolOps ! 1) `multiply` (boolOps ! 2)
  --}
  {--
  putStrLn "Reduced column echelon form of boolean monomial matrix:"
  putStrLn $ printPolyMatBool $ echelonFormBool bmatrix1
  --}
  {--
  putStrLn "The filtration of pointCloud2 is:"
  putStrLn $ filtr2String testFiltration
  --}
  {--
  putStrLn "The boundary operators of the filtration are:"
  putStrLn $ intercalate "\n" $ V.toList $ V.map show $ boundaryOperatorsBool testFiltration
  --}
  {--
  putStrLn "The edge boundary operators of the filtration are:"
  putStrLn $ intercalate "\n" $ V.toList $ V.map show $ edgeBoundaryBool testFiltration
  --}
  {--}
  putStrLn "The bar codes of pointCloud2 are:"
  putStrLn $ intercalate "\n" $ L.map show $ persistentHomology testFiltration
  --}
  {--
  putStrLn "Very simple filtration test:"
  putStrLn $ filtr2String simpleFiltration
  --putStrLn $ intercalate "\n" $ L.map show $ persistentHomology simpleFiltration
  --}
  {--
  putStrLn "Filtration for the octahedron cloud:"
  putStrLn $ filtr2String octahedron
  --}
  {--}
  putStrLn "Bar codes for an octahedron:"
  putStrLn $ intercalate "\n" $ L.map show $ persistentHomology octahedron
  --}
  {--
  putStrLn "VR Complex scale 5 for square cloud:"
  --putStrLn $ show $ makeNbrhdGraph 5.0 metric2 sqrCloud
  putStrLn $ sc2String $ fst $ makeVRComplexFast 3.5 metric2 sqrCloud
  --}
  {--
  putStrLn "Square filtration:"
  putStrLn $ filtr2String square
  --}
  {--}
  putStrLn "Bar codes for a square:"
  putStrLn $ intercalate "\n" $ L.map show $ persistentHomology square
  --}