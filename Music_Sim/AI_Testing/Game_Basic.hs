
import System.Random

data GameState = GState Bool (Int,[(Integer,Integer)]) (Int,[(Integer,Integer)]) | P1Win | P2Win
  deriving Show

data DiceGame = DState Integer [(Integer,Integer)]

data CodedMoves = Prob Integer CodedMoves | Vals [(Integer,Integer)]

data AI = AI StdGen AIDesign

data AIDesign = PickAtRandom | NeuralNetwork [[Double]] | Designed [(Double,String)] [(Double,String)]


basicAI :: Int -> AI
basicAI n = AI (mkStdGen n) (Designed [(0.25,"min"),(0.25,"max"),(0.25,"middle")] [(0.25,"min"),(0.25,"max"),(0.25,"middle"),(0.25,"just beat")])


ioDelayTest :: Integer -> IO ()
ioDelayTest 0 = putStrLn "Done"
ioDelayTest n = do
  putStrLn ("push " ++ (show n))
  name <- getLine
  ioDelayTest (n - 1)

ioGameStart :: Int -> AI -> IO ()
ioGameStart seed ai = ioGameStep (setupGame seed) ai

setupGame :: Int -> GameState
setupGame n = GState True (0,s1) (0,s2) where
  (g1,g2) = split (mkStdGen n)
  s1      = rollDice g1
  s2      = rollDice g2

rollDice :: RandomGen g => g -> [(Integer,Integer)]
rollDice g = roll g [4,6,8,10,12] where
  roll :: RandomGen g => g -> [Integer] -> [(Integer,Integer)]
  roll g []     = []
  roll g (a:as) = (a,b) : (roll g2 as) where
    (b,g2) = randomR (1,a) g

ioGameStep :: GameState -> AI -> IO ()
ioGameStep P1Win _ = putStrLn "Player 1 Wins... WOOO"
ioGameStep P2Win _ = putStrLn "Player 2 Wins... WOOO"
ioGameStep s@(GState True (a,as) (b,bs)) ai = do
  putStrLn ("The score is " ++ show a ++ " to " ++ show b ++ ".")
  putStrLn ("They have " ++ show (fmap fst bs) ++ " you have " ++ show as)
  putStrLn ("Please choose your dice (by first number)")
  input <- getLine
  (x,ai2) <- pure (aiChoice2 ai (read input) bs (fmap fst as))
  putStrLn ("Your opponenet chose " ++ (show x) ++ " the die result was " ++ (show (fst (findVal bs [] x))))
  ioGameStep (stepGameBasic s (read input) x) ai2
ioGameStep s@(GState False (a,as) (b,bs)) ai = do
  (x,ai2) <- pure (aiChoice1 ai bs (fmap fst as))
  putStrLn ("The score is " ++ show a ++ " to " ++ show b ++ ".")
  putStrLn ("They have " ++ show (fmap fst bs) ++ " you have " ++ show as)
  putStrLn ("Your opponenet chose " ++ (show x) ++ " please choose your dice (by first number)")
  input <- getLine
  putStrLn ("Your opponenets die was " ++ (show (fst (findVal bs [] x))))
  ioGameStep (stepGameBasic s (read input) x) ai2


aiChoice1 :: AI -> [(Integer,Integer)] -> [Integer] -> (Integer,AI)
aiChoice1 (AI g PickAtRandom) as _ = (k,AI g2 PickAtRandom) where
  (b,g2) = randomR (0,(length as) - 1) g
  k = fst (as !! b)

aiChoice1 (AI g (Designed xs ys)) as _ = (k, AI g2 (Designed xs ys)) where
  (b,g2) = randomR (0.0,sum (fmap fst xs)) g
  strChoice = (fst . snd) (foldr f (b,("",False)) xs)
  f :: (Double,String) -> (Double,(String,Bool)) -> (Double,(String,Bool))
  f _ (b,(t,True))      = (b,(t,True))
  f (a,s) (b,(t,False)) | a > b     = (a,(s,True))
                        | otherwise = (b - a,(s,False))
  k = designedAI strChoice 0 as




aiChoice2 :: AI -> Integer -> [(Integer,Integer)] -> [Integer] -> (Integer,AI)
aiChoice2 (AI g PickAtRandom) _ as _ = (k,AI g2 PickAtRandom) where
  (b,g2) = randomR (0,(length as) - 1) g
  k = fst (as !! b)

aiChoice2 (AI g (Designed xs ys)) v as _ = (k, AI g2 (Designed xs ys)) where
  (b,g2) = randomR (0.0,sum (fmap fst xs)) g
  strChoice = (fst . snd) (foldr f (b,("",False)) xs)
  f :: (Double,String) -> (Double,(String,Bool)) -> (Double,(String,Bool))
  f _ (b,(t,True))      = (b,(t,True))
  f (a,s) (b,(t,False)) | a > b     = (a,(s,True))
                        | otherwise = (b - a,(s,False))
  k = designedAI strChoice v as




designedAI :: String -> Integer -> [(Integer,Integer)] -> Integer
designedAI "min"   _ as = minimum (fmap fst as)
designedAI "max"   _ as = maximum (fmap fst as)
designedAI "just beat" v as = head ((fmap fst ((dropIf (((>) v) . snd)) as)) ++ (fmap fst as))
designedAI "middle" _ as = lookFor [6,8,4,10,12] (fmap fst as) (fmap fst as)

lookFor :: Eq a => [a] -> [a] -> [a] -> a
lookFor [] _ _  = undefined
lookFor [a] _ _ = a
lookFor (a:as) [] ks = lookFor as ks ks
lookFor (a:as) (b:bs) ks | a == b    = a
                         | otherwise = lookFor (a:as) bs ks

dropIf :: (a -> Bool) -> [a] -> [a]
dropIf _ [] = []
dropIf f (a:as) | (f a)     = dropIf f as
                | otherwise = a : (dropIf f as)

findVal :: [(Integer,Integer)] -> [(Integer,Integer)] -> Integer -> (Integer,[(Integer,Integer)])
findVal [] as _ = (0,as)
findVal ((val,a):as) bs x | val == x  = (a,bs ++ as)
                          | otherwise = findVal as ((val,a) : bs) x

stepGameBasic :: GameState -> Integer -> Integer -> GameState
stepGameBasic P1Win _ _ = P1Win
stepGameBasic P2Win _ _ = P2Win
stepGameBasic (GState _ (3,_) (_,_)) _ _ = P1Win
stepGameBasic (GState _ (_,_) (3,_)) _ _ = P2Win
stepGameBasic s@(GState _ (p1Score, as) (p2Score, bs)) a b = update s (findVal as [] a) (findVal bs [] b) where
  update :: GameState -> (Integer,[(Integer,Integer)]) -> (Integer,[(Integer,Integer)]) -> GameState
  update s@(GState b (p1Score, _) (p2Score, _)) (p1S,as') (p2S,bs') | p1S > p2S = GState True  (p1Score + 1, as') (p2Score,bs')
                                                                    | p2S > p1S = GState False (p1Score, as') (p2Score + 1,bs')
                                                                    | p1S == p2S = GState b (p1Score + 1, as') (p2Score + 1, bs')







doubleScore :: DiceGame -> Integer
doubleScore (DState _ [])     = 0
doubleScore (DState _ [a])    = 0
doubleScore (DState _ (a:as)) = sumSames (snd a) (fmap snd as) + doubleScore (DState _ as) where
  sumSames :: Integer -> [Integer] -> Integer
  sumSames a [] = 0
  sumSames a (b:bs) | a == b    = a + b + sumSames a bs
                    | otherwise = sumSames a bs

maxiProbs :: DiceGame -> Maybe (Integer,Integer)
maxiProbs (DState n as) | n == 0 = Nothing
                        | n == 1 = Just findBestImp as as
                        | otherwise = Just findBestImp as as

expOutput :: [CodedMoves] -> Double
expOutput []     = 0
expOutput (a:as) = calcValue a + expOutput as where
  calcValue :: CodedMoves -> Double
  calcValue (Prob i cm) = (1 / (integerToDouble i)) * (calcValue cm)
  calcValue (Vals as) = doubleScore (DState 0 as)

findAllMoves :: [(Integer,Integer)] -> [(Integer,Integer)]
findAllMoves = findAllPoss 0

findAllPoss :: Integer -> [(Integer,Integer)] -> [(Integer,Integer)]
findAllPoss _ [] = []
findAllPoss n ((a,_):as) = [ (n,x) | x <- [4,6..(a-1)]] ++ (findAllPoss (n + 1) as)

prospectMove :: (Integer,Integer) -> [(Integer,Integer)] -> [(Integer,Integer)] -> [CodedMoves]
prospectMove _ _ [] = Vals []
prospectMove (0,a) ds (b:bs) = [Prob a (Vals (reverse ds ++ (a,x) ++ bs)) | x <- [1..a]]
prospectMove (n,a) ds (b:bs) = prospectMove (n-1,a) (b:ds) bs
