import System.Random

data TownRoad = Road Integer [(Integer,TownRoad)] | PlotR

data TownPlot = Plot [(Integer,Integer)]

data SShape = Triangle (Integer,Integer) (Integer,Integer) | Square (Integer,Integer) (Integer,Integer) (Integer,Integer) | Pentagon (Integer,Integer) (Integer,Integer) (Integer,Integer) (Integer,Integer)

data Graph a = Graph [[a]]
  deriving Show

genGraph :: (RandomGen g, Random a) => Integer -> g -> (a,a) -> Graph a
genGraph numNodes seed lohi = Graph [(chooseRand (stepRand x seed) lohi [1 .. (numNodes - x + 1)]) | x <- [1 .. numNodes] ]

chooseRand :: (RandomGen g, Random a) => g -> (a, a) -> [b] -> [a]
chooseRand _ lohi [] = []
chooseRand g lohi (b:bs) = a : (chooseRand g1 lohi bs) where
  (a,g1) = randomR lohi g


stepRand :: RandomGen g => Integer -> g -> g
stepRand 0 g = g
stepRand n g | mod n 2 == 0 = stepRand (div n 2) (fst (split g))
             | otherwise    = stepRand (div n 2) (snd (split g))
