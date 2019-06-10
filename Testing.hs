{-# LANGUAGE FlexibleInstances #-}

import Util
import Matrix
import SimplicialComplex
import HasseDiagram
import Filtration

import Data.Vector as V
import Data.List as L

class ToString a where
  toString :: a -> String

instance ToString IMatrix where
  toString mat =
    let printVec vec =
          if V.null vec then ""
          else
            let x = V.head vec
            in (show x) L.++ ((if x < 0 then " " else "  ") L.++ (printVec $ V.tail vec))
        print m =
          if V.null m then ""
          else (printVec $ V.head m) L.++ ('\n':(print $ V.tail m))
    in print mat

instance ToString BMatrix where
  toString mat =
    let printVec vec =
          if V.null vec then ""
          else (if V.head vec then "1 " else "0 ") L.++ (printVec $ V.tail vec)
        print m =
          if V.null m then ""
          else (printVec $ V.head m) L.++ ('\n':(print $ V.tail m))
    in print mat

instance Show a => ToString (Vector (Vector (BarCode a))) where
  toString = unlines . V.toList . (V.map show)

data TestResult = Success | Failure String deriving Eq

checkPass :: (Eq a, ToString a) => String -> a -> a -> TestResult
checkPass msg actual expect =
  if actual == expect then Success
  else
    Failure $ msg
      L.++ ":\nActual:\n" L.++ (toString actual)
        L.++ "\nExpected:\n" L.++ (toString expect)

formatTestResults :: [TestResult] -> String
formatTestResults results =
  let numTests  = L.length results
      numPassed = L.length $ L.filter (\r -> r == Success) results
      summary   = (show numPassed) L.++ "/" L.++ (show numTests) L.++ " Tests Passed.\n"
      accum a r =
        case r of
          Success   -> a
          Failure m -> a L.++ [m L.++ "\n"]
      details   = L.foldl accum [] results
  in unlines $ summary:details

matrix1 :: IMatrix
matrix1 = V.fromList $ L.map V.fromList
  [ [ 2, 3, 5]
  , [-4, 2, 3]
  ]

kernel1 :: IMatrix
kernel1 = V.fromList $ L.map V.fromList [[-1, -26, 16]]

matrix2 :: IMatrix
matrix2 = V.fromList $ L.map V.fromList
  [ [1, -1,  3]
  , [5,  6, -4]
  , [7,  4,  2]
  ]

kernel2 :: IMatrix
kernel2 = V.fromList $ L.map V.fromList [[14, -19, -11]]

matrix3 :: IMatrix
matrix3 = V.fromList $ L.map V.fromList $
  [ [1, 0, -3, 0,  2, -8]
  , [0, 1,  5, 0, -1,  4]
  , [0, 0,  0, 1,  7, -9]
  , [0, 0,  0, 0,  0,  0]
  ]

kernel3 :: IMatrix
kernel3 = V.fromList $ L.map V.fromList $
  [ [ 3, -5, 1,  0, 0, 0]
  , [-2,  1, 0, -7, 1, 0]
  , [ 8, -4, 0,  9, 0, 1]
  ]

matrix4 :: IMatrix
matrix4 = V.fromList $ L.map V.fromList
  [ [ 2,  4,   4]
  , [-6,  6,  12]
  , [10, -4, -16]
  ]

snf4 :: IMatrix
snf4 = V.fromList $ L.map V.fromList
  [ [2, 0,  0]
  , [0, 6,  0]
  , [0, 0, 12]
  ]

matrix5 :: IMatrix
matrix5 = V.fromList $ L.map V.fromList $
  [ [  9,  -36,   30]
  , [-36,  192, -180]
  , [ 30, -180,  180]
  ]

snf5 :: IMatrix
snf5 = V.fromList $ L.map V.fromList $
  [ [3, 0, 0]
  , [0, 12, 0]
  , [0, 0, 60]
  ]

matrix6 :: IMatrix
matrix6 = V.fromList $ L.map V.fromList $
  [ [  9, 0, -36,  30]
  , [  0, 0,   0,   0]
  , [-36, 0,  19, -18]
  , [ 30, 0, -18,  18]
  ]

snf6 :: IMatrix
snf6 = V.fromList $ L.map V.fromList $
  [ [ 1, 0,   0, 0]
  , [ 0, 3,   0, 0]
  , [ 0, 0, 462, 0]
  , [ 0, 0,   0, 0]
  ]

pointCloud1 = Right
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

metric2 :: Floating a => (a, a) -> (a, a) -> a
metric2 (a, b) (c, d) =
  let x = a - c; y = b - d
  in sqrt $ x*x + y*y

--8 connected components at index 0, 2 connected components at index 1, 1 connected component at index 2
--1 loop lasting from 0 to 1, 1 loop lasting from 1 to 2, 1 loop starting at 1, 1 loop starting at 2
--will be tested once current bug is found in persistent homology
--scales1 = [6.0, 5.0, 4.0]
scales1 = [10.0, 8.0, 6.0]
testFiltration1 = makeRipsFiltrationFast scales1 metric2 pointCloud1
nsTestFiltration1 = simple2Filtr testFiltration1

octahedralCloud = Right
  [ ( 1, 0, 0)
  , (-1, 0, 0)
  , ( 0, 1, 0)
  , ( 0,-1, 0)
  , ( 0, 0, 1)
  , ( 0, 0,-1)
  ]

metric3 :: Floating a => (a, a, a) -> (a, a, a) -> a
metric3 (x0, y0, z0) (x1, y1, z1) =
  let dx = x1 - x0; dy = y1 - y0; dz = z1 - z0 in
  sqrt (dx*dx + dy*dy + dz*dz)

octaScales :: [Double]
octaScales = [2.1, 1.6, 1.1, 0.5]

--5 connected components from 0 to 2, 1 void from 2 to 3
octahedron = makeRipsFiltrationFast octaScales metric3 octahedralCloud
nsoctahedron = simple2Filtr octahedron

sqrCloud = Right
  [ (0, 0)
  , (1, 0)
  , (2, 0)
  , (3, 0)
  , (3, 1)
  , (3, 2)
  , (3, 3)
  , (2, 3)
  , (1, 3)
  , (0, 3)
  , (0, 2)
  , (0, 1)
  ]

sqrScales :: [Double]
sqrScales = [2.0, 1.5, 1.1, 0.5]

--11 connected components from 0 to 1, 1 loop starting at 1
square = makeRipsFiltrationFast sqrScales metric2 sqrCloud
nssquare = simple2Filtr square

dGraph1 =
  [ ( 0,  1)
  , ( 0,  2)
  , ( 1,  2)
  , ( 1,  3)
  , ( 1,  4)
  , ( 1,  6)
  , ( 1,  7)
  , ( 2,  4)
  , ( 2,  6)
  , ( 2,  7)
  , ( 3,  2)
  , ( 3,  4)
  , ( 3,  6)
  , ( 3,  7)
  , ( 4,  5)
  , ( 4,  6)
  , ( 4,  7)
  , ( 5,  9)
  , ( 5, 11)
  , ( 6,  7)
  , ( 6, 10)
  , ( 6, 12)
  , ( 7,  8)
  , ( 7, 12)
  , ( 8,  4)
  , ( 8,  5)
  , ( 8,  9)
  , (11,  8)
  , (11,  9)
  , (11, 14)
  , (12, 10)
  , (12, 13)
  , (13, 15)
  , (13, 17)
  , (14, 13)
  , (14, 15)
  , (14, 16)
  , (14, 17)
  , (16, 13)
  , (16, 15)
  , (16, 17)
  , (17, 15)
  ]

directedGraph = encodeDirectedGraph 18 dGraph1
dCliqueComplex = toSimplicialComplex $ directedFlagComplex directedGraph

--Mobius strip created in blender
mobius = Right
  [ ( 1.250000,  0.000000,  0.000000)
  , ( 0.750000,  0.000000,  0.000000)
  , (-1.250000, -0.000000, -0.000000)
  , (-1.199179,  0.064705,  0.321319)
  , (-1.053526,  0.125000,  0.608253)
  , (-0.832107,  0.176777,  0.832107)
  , (-0.562500,  0.216506,  0.974279)
  , (-0.275566,  0.241481,  1.028426)
  , ( 0.000000,  0.250000,  1.000000)
  , ( 0.242072,  0.241481,  0.903426)
  , ( 0.437500,  0.216506,  0.757772)
  , ( 0.582106,  0.176777,  0.582107)
  , ( 0.678525,  0.125000,  0.391747)
  , ( 0.732673,  0.064705,  0.196319)
  , (-1.207407, -0.000000, -0.323524)
  , (-1.082532, -0.000000, -0.625000)
  , (-0.883884, -0.000000, -0.883884)
  , (-0.625000, -0.000000, -1.082532)
  , (-0.323524, -0.000000, -1.207408)
  , ( 0.000000, -0.000000, -1.250001)
  , ( 0.323524, -0.000000, -1.207408)
  , ( 0.625001, -0.000000, -1.082533)
  , ( 0.883884, -0.000000, -0.883884)
  , ( 1.082533, -0.000000, -0.625000)
  , ( 1.207409, -0.000000, -0.323524)
  , (-0.258819,  0.000000,  0.965926)
  , (-0.562438, -0.099169,  0.732983)
  , ( 0.562437,  0.099169,  0.732983)
  , (-0.707107,  0.000000,  0.707107)
  , ( 0.000000,  0.000000,  1.000000)
  , (-1.030574,  0.047835,  0.426877)
  , ( 1.000000,  0.000000,  0.000000)
  , ( 0.868575,  0.016316,  0.114350)
  , (-1.114315,  0.016316,  0.146703)
  , ( 0.965926,  0.000000,  0.258819)
  , ( 1.030574, -0.047835,  0.426878)
  , (-0.872030,  0.076095,  0.669132)
  , ( 0.872030, -0.076095,  0.669132)
  , ( 0.707106,  0.000000,  0.707107)
  , (-0.400989,  0.115485,  0.968074)
  , ( 0.128396,  0.123931,  0.975269)
  , ( 0.364378,  0.115485,  0.879685)
  , (-0.128397, -0.123931,  0.975268)
  , (-1.115376, -0.000000, -0.146842)
  , (-1.039365, -0.000000, -0.430519)
  , (-0.866026, -0.000000, -0.500000)
  , (-0.694184,  0.000000, -0.532666)
  , (-0.684857, -0.000000, -0.892523)
  , (-0.500000, -0.000000, -0.866026)
  , (-0.334848,  0.000000, -0.808395)
  , (-0.146842, -0.000000, -1.115376)
  , ( 0.000000, -0.000000, -1.000000)
  , ( 0.114211,  0.000000, -0.867515)
  , ( 0.430519, -0.000000, -1.039365)
  , ( 0.500000, -0.000000, -0.866026)
  , ( 0.532667,  0.000000, -0.694185)
  , ( 0.892523, -0.000000, -0.684857)
  , ( 0.866026, -0.000000, -0.500000)
  , ( 0.808395,  0.000000, -0.334848)
  , ( 1.115377, -0.000000, -0.146842)
  ]

mobScales = [0.5, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2, 0.15]
mobiusStrip = makeRipsFiltrationFast mobScales metric3 mobius
nsmobius = simple2Filtr mobiusStrip

tests =
  [ let actual = kernelInt matrix1
        expect = kernel1
    in checkPass "kernelInt matrix1" actual expect

  , let actual = kernelIntPar matrix1
        expect = kernel1
    in checkPass "kernelIntPar matrix1" actual expect

  , let actual = kernelInt matrix2
        expect = kernel2
    in checkPass "kernelInt matrix2" actual expect

  , let actual = kernelIntPar matrix2
        expect = kernel2
    in checkPass "kernelIntPar matrix2" actual expect

  , let actual = kernelInt matrix3
        expect = kernel3
    in checkPass "kernelInt matrix3" actual expect

  , let actual = kernelIntPar matrix3
        expect = kernel3
    in checkPass "kernelIntPar matrix3" actual expect

  , let actual = normalFormInt matrix4
        expect = snf4
    in checkPass "normalFormInt matrix4" actual expect

  , let actual = normalFormIntPar matrix4
        expect = snf4
    in checkPass "normalFormIntPar matrix4" actual expect

  , let actual = normalFormInt matrix5
        expect = snf5
    in checkPass "normalFormInt matrix5" actual expect

  , let actual = normalFormIntPar matrix5
        expect = snf5
    in checkPass "normalFormIntPar matrix5" actual expect

  , let actual = normalFormInt matrix6
        expect = snf6
    in checkPass "normalFormInt matrix6" actual expect

  , let actual = normalFormIntPar matrix6
        expect = snf6
    in checkPass "normalFormIntPar matrix6" actual expect

  --octahedral filtration

  , let actual = indexBarCodesSimple octahedron
        expect = V.fromList $ L.map V.fromList [[(0, Finite 2), (0, Finite 2), (0, Finite 2), (0, Finite 2), (0,Finite 2),(0,Infinity)], [], [(2,Finite 3)], [], []]
    in checkPass "indexBarCodesSimple octahedron" actual expect

  , let actual = scaleBarCodesSimple (Right octaScales) octahedron
        expect = V.fromList $ L.map V.fromList [[(0.5, Finite 1.6),(0.5, Finite 1.6),(0.5, Finite 1.6),(0.5, Finite 1.6),(0.5, Finite 1.6), (0.5, Infinity)], [], [(1.6,Finite 2.1)], [], []]
    in checkPass "scaleBarCodesSimple octahedron" actual expect

  , let actual = indexBarCodes nsoctahedron
        expect = V.fromList $ L.map V.fromList [[(0, Finite 2), (0, Finite 2), (0, Finite 2), (0, Finite 2), (0,Finite 2),(0,Infinity)], [], [(2,Finite 3)], [], [], []]
    in checkPass "indexBarCodes octahedron" actual expect

  , let actual = scaleBarCodes (Right octaScales) nsoctahedron
        expect = V.fromList $ L.map V.fromList [[(0.5, Finite 1.6),(0.5, Finite 1.6),(0.5, Finite 1.6),(0.5, Finite 1.6),(0.5, Finite 1.6), (0.5, Infinity)], [], [(1.6,Finite 2.1)], [], [], []]
    in checkPass "scaleBarCodes octahedron" actual expect

  --square filtration

  , let actual = indexBarCodesSimple square
        expect = V.fromList $ L.map V.fromList [[(0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Infinity)], [(1,Infinity)]]
    in checkPass "indexBarCodesSimple square" actual expect

  , let actual = scaleBarCodesSimple (Right sqrScales) square
        expect = V.fromList $ L.map V.fromList [[(0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Infinity)], [(1.1,Infinity)]]
    in checkPass "scaleBarCodesSimple square" actual expect

  , let actual = indexBarCodes nssquare
        expect = V.fromList $ L.map V.fromList [[(0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Finite 1), (0, Infinity)], [(1,Infinity)], []]
    in checkPass "indexBarCodes square" actual expect

  , let actual = scaleBarCodes (Right sqrScales) nssquare
        expect = V.fromList $ L.map V.fromList [[(0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Finite 1.1), (0.5, Infinity)], [(1.1,Infinity)], []]
    in checkPass "scaleBarCodes square" actual expect

  --mobius strip filtration
  --the mobius strip has a 1-cycle which isn't being picked up by the algorithm
  --so we print all the test results every time

  , let actual = indexBarCodesSimple mobiusStrip
        expect = V.empty
    in checkPass "indexBarCodesSimple mobius" actual expect

  , let actual = scaleBarCodesSimple (Right mobScales) mobiusStrip
        expect = V.empty
    in checkPass "scaleBarCodesSimple mobius" actual expect

  , let actual = indexBarCodes nsmobius
        expect = V.empty
    in checkPass "indexBarCodes mobius" actual expect

  , let actual = scaleBarCodes (Right mobScales) nsmobius
        expect = V.empty
    in checkPass "scaleBarCodes mobius" actual expect

  ]

main = putStrLn $ formatTestResults tests
