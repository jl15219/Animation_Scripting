
import Arr
import CsvFileConverter
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import BlenderScript
import System.Random


peaksFresh :: Arr [Double] -> Arr Double
peaksFresh bss = peaksChange (reset 0 bss) bss

peaksChange :: Arr Double -> Arr [Double] -> Arr Double
peaksChange ass bss = arrTranDegFast ass bss f (incByNext (+)) where
  f :: Double -> [Double] -> Double
  f a (b:bs) = max a b

basicTest :: Integer -> Arr Double -> Arr Double -> (Arr Double, Arr Double)
basicTest n ass bss = arrTranDegTest n ass bss (max) f where
  f a = a - 1

arr2CSV :: Arr Double -> String
arr2CSV = p . q . h . g . f where
  f :: Arr Double -> Arr Double
  f = pushThrough (fmap shift2Front)
  g :: Arr Double -> Arr Double
  g = shift2Front
  h :: Arr Double -> Arr String
  h = pushThrough (fmap (pushThrough (fmap (show))))
  q :: Arr String -> [[String]]
  q = snd . (pushThrough (fmap (snd)))
  p :: [[String]] -> String
  p = foldr foldingFunc ""
  foldingFunc :: [String] -> String -> String
  foldingFunc as b = (foldr ff2 "" as) ++ "\n" ++ b
  ff2 :: String -> String -> String
  ff2 a b = "\"" ++ a ++ "\"," ++ b


turnPeaksToHeights :: String -> IO ()
turnPeaksToHeights fileName = do
  file <- readFile (txtFile fileName)
  writeFile (csvFile (fileName ++ "Result")) $ case parse parseArr (txtFile fileName) file of
    Left err   -> "ERROR\n"
    Right prog -> (arr2CSV . peaksFresh) prog

postPeaksAsHeights :: String -> IO ()
postPeaksAsHeights fn = do
  file <- readFile (txtFile fn)
  putStrLn $ case parse parseArr (txtFile fn) file of
    Left err   -> show err
    Right prog -> show (peaksFresh prog)


stdnArr :: Integer -> Arr Double
stdnArr 000 = ([],[ ([],[0,0,0]), ([],[0,0,0]), ([],[0,0,0]) ])
stdnArr 303 = ([],[ ([],[1,2,3]), ([],[4,5,6]), ([],[7,8,9]) ])

peakArr :: Integer -> Arr [Double]
peakArr 303 = ([],[ ([],[[1,0],[2,-1],[3,-2]]), ([],[[4,-3],[5,-2],[6,-4]]), ([],[[7,-3],[8,-4],[9,-5]])])

doMult :: Integer -> (a -> a) -> (a -> a)
doMult 1 f = f
doMult n f = f . (doMult (n-1) f)

txtFile :: String -> FilePath
txtFile s = s ++ ".txt"

-- Do multple loopcuts and then highlight all vertices and then click edit mode ---> highlight all ---> mesh ---> sorting ---> sort by x axis


peaksToPython :: String -> String -> IO ()
peaksToPython fileName object = do
  file <- readFile (txtFile fileName)
  writeFile (txtFile (fileName ++ "BScript")) $ case parse parseArr (txtFile fileName) file of
    Left err   -> "ERROR\n"
    Right prog -> (unlines . (["import bpy"] ++) . (fmap (\(a,b) -> setZVert object a b)) . (zip [0..]) . (fmap (/10)) . concat . arrToLofL . peaksFresh) prog


list2Rand :: RandomGen g => g -> [a] -> [Integer]
list2Rand _ []     = []
list2Rand seed (a:as) = k : (list2Rand g as) where (k,g) = randomR (0,100) seed

sls2Rand :: RandomGen g => g -> SLS a -> SLS Integer
sls2Rand seed (as,bs) = (list2Rand g1 as, list2Rand g2 bs) where (g1,g2) = split seed

slsList2Rand :: RandomGen g => g -> [SLS a] -> [SLS Integer]
slsList2Rand _ [] = []
slsList2Rand seed (a:as) = (sls2Rand g1 a) : (slsList2Rand g2 as) where (g1,g2) = split seed

arr2Rand :: RandomGen g => g -> Arr a -> Arr Integer
arr2Rand seed (ass,bss) = (slsList2Rand g1 ass, slsList2Rand g2 bss) where (g1,g2) = split seed


randArrBool :: Int -> Arr Integer -> Arr Bool
randArrBool seed ass = zipWithArr (<) (arr2Rand (mkStdGen seed) ass) ass

postRandDist :: String -> IO ()
postRandDist fn = do
  file <- readFile (csvFile fn)
  putStrLn $ case parse parseArr (csvFile fn) file of
    Left err   -> show err
    Right prog -> show (peaksFresh prog)

makeRandDist :: String -> IO ()
makeRandDist fileName = do
  file <- readFile (csvFile fileName)
  writeFile (csvFile (fileName ++ "Result")) $ case parse parseArr (csvFile fileName) file of
    Left err   -> "ERROR\n"
    Right prog -> (arr2CSV . peaksFresh) prog

randDist2Python :: String -> Int -> IO ()
randDist2Python fileName seed = do
  file <- readFile (csvFile fileName)
  writeFile (txtFile (fileName ++ "BScript")) $ case parse parseCSV (csvFile fileName) file of
    Left err   -> "ERROR\n"
    Right prog -> (unlines . (["import bpy"] ++) . (pyRand) . (conZip) . (fmap (zip [0..])) . arrToLofL . (randArrBool seed) . (fmapArr (round))) prog

randDistShow :: String -> Int -> IO ()
randDistShow fileName seed = do
  file <- readFile (csvFile fileName)
  putStrLn $ case parse parseCSV (csvFile fileName) file of
    Left err   -> "ERROR\n"
    Right prog -> show ((fmapArr (round)) prog)


conZip :: [[(Integer,Bool)]] -> [(Integer,(Integer,Bool))]
conZip = concat . fmap (\(x,as) -> zip [x,x..] as) . zip [0..]

pyRand :: [(Integer,(Integer,Bool))] -> [String]
pyRand [] = []
pyRand ((_,(_,False)):as) = pyRand as
pyRand ((x,(y,True)):as) = pasteCopied : (transXYZ (fromInteger x, fromInteger y,0)) : (pyRand as)
