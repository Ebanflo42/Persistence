import Util
import Matrix
import SimplicialComplex
import HasseDiagram
import Filtration

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

pointCloud1 = Right
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

pointCloud2 = Right
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

octahedralCloud = Right
  [ ( 1, 0, 0)
  , (-1, 0, 0)
  , ( 0, 1, 0)
  , ( 0,-1, 0)
  , ( 0, 0, 1)
  , ( 0, 0,-1)
  ]

sqrCloud = Right
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

mobius = Right
  [ ( 1.250000,  0.000000,  0.000000)
  , ( 0.750000,  0.000000,  0.000000)
  , (-0.750000,  0.000000, -0.000000)
  , (-1.250000, -0.000000, -0.000000)
  , (-0.732673, -0.064705,  0.196319)
  , (-1.199179,  0.064705,  0.321319)
  , (-0.678526, -0.125000,  0.391747)
  , (-1.053526,  0.125000,  0.608253)
  , (-0.582107, -0.176777,  0.582107)
  , (-0.832107,  0.176777,  0.832107)
  , (-0.437500, -0.216506,  0.757772)
  , (-0.562500,  0.216506,  0.974279)
  , (-0.242072, -0.241481,  0.903426)
  , (-0.275566,  0.241481,  1.028426)
  , ( 0.000000, -0.250000,  1.000000)
  , ( 0.000000,  0.250000,  1.000000)
  , ( 0.275565, -0.241481,  1.028426)
  , ( 0.242072,  0.241481,  0.903426)
  , ( 0.562500, -0.216506,  0.974279)
  , ( 0.437500,  0.216506,  0.757772)
  , ( 0.832106, -0.176777,  0.832107)
  , ( 0.582106,  0.176777,  0.582107)
  , ( 1.053525, -0.125000,  0.608253)
  , ( 0.678525,  0.125000,  0.391747)
  , ( 1.199179, -0.064705,  0.321319)
  , ( 0.732673,  0.064705,  0.196319)
  , (-0.724444,  0.000000, -0.194114)
  , (-1.207407, -0.000000, -0.323524)
  , (-0.649519,  0.000000, -0.375000)
  , (-1.082532, -0.000000, -0.625000)
  , (-0.530330,  0.000000, -0.530330)
  , (-0.883884, -0.000000, -0.883884)
  , (-0.375000,  0.000000, -0.649519)
  , (-0.625000, -0.000000, -1.082532)
  , (-0.194114,  0.000000, -0.724445)
  , (-0.323524, -0.000000, -1.207408)
  , ( 0.000000,  0.000000, -0.750000)
  , ( 0.000000, -0.000000, -1.250001)
  , ( 0.194114,  0.000000, -0.724445)
  , ( 0.323524, -0.000000, -1.207408)
  , ( 0.375000,  0.000000, -0.649519)
  , ( 0.625001, -0.000000, -1.082533)
  , ( 0.530331,  0.000000, -0.530330)
  , ( 0.883884, -0.000000, -0.883884)
  , ( 0.649520,  0.000000, -0.375000)
  , ( 1.082533, -0.000000, -0.625000)
  , ( 0.724445,  0.000000, -0.194114)
  , ( 1.207409, -0.000000, -0.323524)
  , ( 0.258819, -0.000000,  0.965926)
  , (-0.258819,  0.000000,  0.965926)
  , (-0.500000,  0.000000,  0.866026)
  , (-0.562438, -0.099169,  0.732983)
  , (-0.655086,  0.099169,  0.853723)
  , ( 0.562437,  0.099169,  0.732983)
  , ( 0.655085, -0.099169,  0.853724)
  , (-0.707107,  0.000000,  0.707107)
  , (-0.866026,  0.000000,  0.500000)
  , ( 0.000000,  0.000000,  1.000000)
  , ( 0.866025,  0.000000,  0.500000)
  , (-1.030574,  0.047835,  0.426877)
  , (-0.817186, -0.047835,  0.338489)
  , ( 1.000000,  0.000000,  0.000000)
  , (-1.000000, -0.000000, -0.000000)
  , ( 0.868575,  0.016316,  0.114350)
  , ( 1.114315, -0.016316,  0.146702)
  , (-1.114315,  0.016316,  0.146703)
  , (-0.868575, -0.016316,  0.114350)
  , ( 0.965926,  0.000000,  0.258819)
  , (-0.965926,  0.000000,  0.258819)
  , ( 1.030574, -0.047835,  0.426878)
  , ( 0.817185,  0.047835,  0.338490)
  , (-0.872030,  0.076095,  0.669132)
  , (-0.714677, -0.076095,  0.548391)
  , ( 0.872030, -0.076095,  0.669132)
  , ( 0.714677,  0.076095,  0.548391)
  , ( 0.707106,  0.000000,  0.707107)
  , (-0.364378, -0.115485,  0.879685)
  , (-0.400989,  0.115485,  0.968074)
  , ( 0.500000,  0.000000,  0.866025)
  , ( 0.128396,  0.123931,  0.975269)
  , ( 0.132656, -0.123931,  1.007621)
  , ( 0.364378,  0.115485,  0.879685)
  , ( 0.400989, -0.115485,  0.968074)
  , (-0.128397, -0.123931,  0.975268)
  , (-0.132656,  0.123931,  1.007621)
  , (-1.115376, -0.000000, -0.146842)
  , (-0.867514,  0.000000, -0.114211)
  , (-0.965926, -0.000000, -0.258819)
  , (-1.039365, -0.000000, -0.430519)
  , (-0.808395,  0.000000, -0.334848)
  , (-0.866026, -0.000000, -0.500000)
  , (-0.892523, -0.000000, -0.684857)
  , (-0.694184,  0.000000, -0.532666)
  , (-0.707107, -0.000000, -0.707107)
  , (-0.684857, -0.000000, -0.892523)
  , (-0.532666,  0.000000, -0.694184)
  , (-0.500000, -0.000000, -0.866026)
  , (-0.430519, -0.000000, -1.039365)
  , (-0.334848,  0.000000, -0.808395)
  , (-0.258819, -0.000000, -0.965926)
  , (-0.146842, -0.000000, -1.115376)
  , (-0.114210,  0.000000, -0.867515)
  , ( 0.000000, -0.000000, -1.000000)
  , ( 0.146842, -0.000000, -1.115376)
  , ( 0.114211,  0.000000, -0.867515)
  , ( 0.258819, -0.000000, -0.965926)
  , ( 0.430519, -0.000000, -1.039365)
  , ( 0.334848,  0.000000, -0.808395)
  , ( 0.500000, -0.000000, -0.866026)
  , ( 0.684857, -0.000000, -0.892523)
  , ( 0.532667,  0.000000, -0.694185)
  , ( 0.707107, -0.000000, -0.707107)
  , ( 0.892523, -0.000000, -0.684857)
  , ( 0.694185,  0.000000, -0.532667)
  , ( 0.866026, -0.000000, -0.500000)
  , ( 1.039366, -0.000000, -0.430519)
  , ( 0.808395,  0.000000, -0.334848)
  , ( 0.965927, -0.000000, -0.258819)
  , ( 1.115377, -0.000000, -0.146842)
  , ( 0.867515, 0.000000 , -0.114210)
  ]

metric2 :: (Float, Float) -> (Float, Float) -> Float
metric2 (a, b) (c, d) =
  let x = a - c; y = b - d in
  sqrt (x*x + y*y)

metric3 :: (Float, Float, Float) -> (Float, Float, Float) -> Float
metric3 (x0, y0, z0) (x1, y1, z1) =
  let dx = x1 - x0; dy = y1 - y0; dz = z1 - z0 in
  sqrt (dx*dx + dy*dy + dz*dz)

testFiltration1 = makeRipsFiltrationFast [10.0, 8.0, 6.0] metric2 pointCloud1
nstestFiltration1 = simple2Filtr testFiltration1

--8 connected components at index 0, 2 connected components at index 1, 1 connected component at index 2
--1 loop lasting from 0 to 1, 1 loop lasting from 1 to 2, 1 loop starting at 1, 1 loop starting at 2
testFiltration2 = makeRipsFiltrationFast [6.0, 5.0, 4.0] metric2 pointCloud2
nsTestFiltration2 = simple2Filtr testFiltration2

--5 connected components from 0 to 2, 1 void from 2 to 3
octahedron = makeRipsFiltrationFast [2.1, 1.6, 1.1, 0.5] metric3 octahedralCloud

--11 connected components from 0 to 1, 1 loop starting at 1
square = makeRipsFiltrationFast [1.5, 1.0, 0.5] metric2 sqrCloud

mobiusStrip = makeRipsFiltrationFast [0.3, 0.25, 0.2, 0.15] metric3 mobius

directedGraph = encodeDirectedGraph 18 dGraph1

dCliqueComplex = toSimplicialComplex $ directedFlagComplex directedGraph

main = do
{--
  putStrLn "The first matrix is:"
  putStrLn $ iMat2String matrix1
  putStrLn "It's kernel is:"
  putStrLn $ iMat2String $ kernelInt matrix1
  putStrLn "It's kernel computed in parallel is:"
  putStrLn $ iMat2String $ kernelIntPar matrix1
  putStrLn "The kernel should be:"
  putStrLn $ iMat2String kernel1

  putStrLn "The second matrix is:"
  putStrLn $ iMat2String matrix2
  putStrLn "It's kernel is:"
  putStrLn $ iMat2String $ kernelInt matrix2
  putStrLn "It's kernel computed in parallel is:"
  putStrLn $ iMat2String $ kernelIntPar matrix2
  putStrLn "The kernel should be:"
  putStrLn $ iMat2String kernel2

  putStrLn "The third matrix is:"
  putStrLn $ iMat2String matrix3
  putStrLn "It's kernel is:"
  putStrLn $ iMat2String $ kernelInt matrix3
  putStrLn "It's kernel computed in parallel is:"
  putStrLn $ iMat2String $ kernelIntPar matrix3
  putStrLn "The kernel should be:"
  putStrLn $ iMat2String kernel3

  putStrLn "The fourth matrix is:"
  putStrLn $ iMat2String matrix4
  putStrLn "It's Smith normal form is:"
  putStrLn $ iMat2String $ normalFormInt matrix4
  putStrLn "It's Smith normal form computed in parallel is:"
  putStrLn $ iMat2String $ normalFormIntPar matrix4
  putStrLn "The Smith Normal form should be:"
  putStrLn $ iMat2String $ snf4

  putStrLn "The fifth matrix is:"
  putStrLn $ iMat2String matrix5
  putStrLn "It's Smith normal form is:"
  putStrLn $ iMat2String $ normalFormInt matrix5
  putStrLn "It's Smith normal form computed in parallel is:"
  putStrLn $ iMat2String $ normalFormIntPar matrix5
  putStrLn "The Smith Normal form should be:"
  putStrLn $ iMat2String $ snf5

  putStrLn "The sixth matrix is:"
  putStrLn $ iMat2String matrix6
  putStrLn "It's Smith normal form is:"
  putStrLn $ iMat2String $ normalFormInt matrix6
  putStrLn "It's Smith normal form computed in parallel is:"
  putStrLn $ iMat2String $ normalFormIntPar matrix6
  --}

  {--
  putStrLn "Index bar codes of point cloud 1:"
  let ipcs1 = indexBarCodesSimple testFiltration1
  putStrLn $ (intercalate "\n" $ L.map show $ L.map (L.filter (\(a, b) -> b /= Finite a)) ipcs1) L.++ "\n"
  --}
  {--
  putStrLn "Scale bar codes of the point cloud 1:"
  let spcs1 = scaleBarCodesSimple [10.0, 8.0, 6.0] testFiltration1
  putStrLn $ (intercalate "\n" $ L.map show spcs1) L.++ "\n"
  --}
  {--
  putStrLn "Index bar codes of point cloud 1 (non-simple):"
  let ipcs1 = indexBarCodes nstestFiltration1
  putStrLn $ (intercalate "\n" $ L.map show $ L.map (L.filter (\(a, b) -> b /= Finite a)) ipcs1) L.++ "\n"
  --}
  {--
  putStrLn "Scale bar codes of the point cloud 1 (non-simple):"
  let spcns1 = scaleBarCodes [10.0, 8.0, 6.0] nstestFiltration1
  putStrLn $ (intercalate "\n" $ L.map show spcns1) L.++ "\n"
  --}

  {--
  putStrLn "Index bar codes of point cloud 2:"
  let ipcs2 = indexBarCodesSimple testFiltration2
  putStrLn $ (intercalate "\n" $ L.map show $ L.map (L.filter (\(a, b) -> b /= Finite a)) ipcs2) L.++ "\n"
  --}
  {--
  putStrLn "Scale bar codes of the point cloud 2:"
  let spcs2 = scaleBarCodesSimple [6.0, 5.0, 4.0] testFiltration2
  putStrLn $ (intercalate "\n" $ L.map show spcs2) L.++ "\n"
  --}
  {--
  putStrLn "Index bar codes of point cloud 2 as a non-simple filtration:"
  let ipcns2 = indexBarCodes nsTestFiltration2
  putStrLn $ (intercalate "\n" $ L.map show ipcns2) L.++ "\n"
  --}
  {--
  putStrLn "Scale bar codes of point cloud 2 as a non-simple filtration:"
  let sbcns = scaleBarCodes [6.0, 5.0, 4.0] nsTestFiltration2
  putStrLn $ (intercalate "\n" $ L.map show sbcns) L.++ "\n"
  --}

  {--
  putStrLn "Index bar codes of the octahedral cloud (simple):"
  let iocts = indexBarCodesSimple octahedron
  putStrLn $ (intercalate "\n" $ L.map show iocts) L.++ "\n"
  --}
  {--
  putStrLn "Scale bar codes of the octahedral cloud (simple):"
  let socts = scaleBarCodesSimple [2.1, 1.6, 1.1, 0.5] octahedron
  putStrLn $ (intercalate "\n" $ L.map show socts) L.++ "\n"
  --}
  {--
  putStrLn "Index bar codes of the octahedral cloud (non-simple):"
  let ioctns = indexBarCodes $ simple2Filtr octahedron
  putStrLn $ (intercalate "\n" $ L.map show ioctns) L.++ "\n"
  --}
  {--
  putStrLn "Scale bar codes of the octahedral cloud (non-simple):"
  let soctns = scaleBarCodes [2.1, 1.6, 1.1, 0.5] $ simple2Filtr octahedron
  putStrLn $ (intercalate "\n" $ L.map show soctns) L.++ "\n"
  --}

  {--
  putStrLn "Index bar codes of the square cloud (simple):"
  let isqrs = indexBarCodesSimple square
  putStrLn $ (intercalate "\n" $ L.map show isqrs) L.++ "\n"
  --}
  {--
  putStrLn "Scale bar codes of the square cloud (simple):"
  let ssqrs = scaleBarCodesSimple [1.5, 1.0, 0.5] square
  putStrLn $ (intercalate "\n" $ L.map show ssqrs) L.++ "\n"
  --}
  {--
  putStrLn "Index bar codes of the square cloud (non-simple):"
  let isqrns = indexBarCodes $ simple2Filtr square
  putStrLn $ (intercalate "\n" $ L.map show isqrns) L.++ "\n"
  --}
  {--
  putStrLn "Scale bar codes of the square cloud (non-simple):"
  let ssqrns = scaleBarCodes [1.5, 1.0, 0.5] $ simple2Filtr square
  putStrLn $ (intercalate "\n" $ L.map show ssqrns) L.++ "\n"
  --}
  {--}
  putStrLn "Index bar codes of the square cloud with cycle vertices (non-simple):"
  let vertexTest = indexBarCodeVertices $ simple2Filtr square
  putStrLn $ (intercalate "\n" $ L.map show vertexTest) L.++ "\n"
  --}

  {--
  putStrLn "Index bar codes of Mobius strip (simple):"
  let imobs = indexBarCodesSimple mobiusStrip
  putStrLn $ (intercalate "\n" $ L.map show imobs) L.++ "\n"
  --}
  {--
  putStrLn "Scale bar codes of Mobius strip (simple):"
  let smobs = scaleBarCodesSimple [0.3, 0.25, 0.2, 0.15] mobiusStrip
  putStrLn $ (intercalate "\n" $ L.map show smobs) L.++ "\n"
  --}
  {--
  putStrLn "Index bar codes of Mobius strip (non-simple):"
  let imobns = indexBarCodes $ simple2Filtr mobiusStrip
  putStrLn $ (intercalate "\n" $ L.map show imobns) L.++ "\n"
  --}
  {--
  putStrLn "Scale bar codes of Mobius strip (non-simple):"
  let smobns = scaleBarCodes [0.3, 0.25, 0.2, 0.15] $ simple2Filtr mobiusStrip
  putStrLn $ (intercalate "\n" $ L.map show smobns) L.++ "\n"
  --}

  {--
  putStrLn "Bottleneck distances between point cloud 1 & 2:"
  let pc12 = bottleNeckDistances indexMetric ipcs1 ipcs2
  putStrLn $ (intercalate "\n" $ L.map show pc12) L.++ "\n"
  --}
  {--
  putStrLn "Bottleneck distances between octahedron barcodes and square barcodes:"
  let sqroct = bottleNeckDistances indexMetric iocts isqrs
  putStrLn $ (intercalate "\n" $ L.map show sqroct) L.++ "\n"
  --}