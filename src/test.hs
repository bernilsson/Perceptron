import System.Random (randomRIO)
import Data.Array.IO
import Control.Monad


updateWeights :: Double -> Double -> [Double] -> [Double] -> [Double]
updateWeights learnRate error inputs weights =
    zipWith (\x y -> x * y * learnRate * error) inputs weights


step :: Double -> Bool
step n = if n > 0 then 1 else 0


percieveFace :: Image -> Face
percieveFace img eyesWeigths mouthWeights actfn =
    getFace (percieve img eyesWeigths actfn) (percieve img mouthWeights actfn)

-- ska percieve returnera true false, eller en Double?
percieve :: [Double] -> [Double] -> (Double -> Bool) -> Bool
percieve inputs weights actfn = actfn $ sum $ zipWith (*) inputs weights

type Image = [Double]
type Face = Int -- 1 = Happy, 2 = Sad, 3 = Mischievous, 4 = Mad
type Eyes = Bool
type Mouth = Bool

-- getEyesMouth returns the state of the eyes and mouth, given the face type.
-- \ / up    brows = True  / \ down brows = False
-- \_/ happy mouth = True  /-\ sad mouth  = False
getEyesMouth :: Face -> (Eyes,Mouth)
getEyesMouth 1 = (False,True)  -- Happy
getEyesMouth 2 = (False,False) -- Sad
getEyesMouth 3 = (True,False)  -- Mischievous
getEyesMouth 4 = (True,True)   -- Mad

getFace :: Eyes -> Mouth -> Face
getFace (False,True)  = 1 -- Happy
getFace (False,False) = 2 -- Sad
getFace (True,False)  = 3 -- Mischievous
getFace (True,True)   = 4 -- Mad

trainFace :: [(Image,Face)] ->
trainFace learnRate (img,face)

-- train takes a list of image and answer pairs to train on, the weights of the
-- output node that should be trained, and the learning rate. It returns updated
-- weights.
train :: Double -> [(Image,Bool)] -> [Double] -> [Double]
train learnRate [] weights = weights -- nothing left to train on
train learnRate ((img,ans):xs) weights =
    if percieve img == ans
        then train xs weights
        else train xs (update weights)
    where
        percieve img = percieve img weights step
        update weigths = updateWeights learnRate 1 img ws

-----------------------------------------------------
-- Flippin' images
-----------------------------------------------------

-- flipCW flips a  image clockwise
flipCW :: Image -> Int -> Image
flipCW img width = concat $ flip (chunk img width) []
    where flip :: [[Double]] -> [[Double]]
          flip []  out = out
          flip img out = flip (map tail img) (map head img)

flip180 = reverse

-- Is it worth it, let me work it. I put my thing down, flip it and reverse it
flipCCW img width = flip180 . flipCW

-- http://www.haskell.org/haskellwiki/Random_shuffle
-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do ar <- newArray n xs
                forM [1..n] $ \i -> do
                    j <- randomRIO (i,n)
                    vi <- readArray ar i
                    vj <- readArray ar j
                    writeArray ar j vi
                    return vj
             where
                 n = length xs
                 newArray :: Int -> [a] -> IO (IOArray Int a)
                 newArray n xs =  newListArray (1,n) xs


---------------------------------------------
-- Trash:
---------------------------------------------



train2 :: Double -> [Image] -> [Bool] -> [Double] -> [Double]
train2 learnRate [] _ weights = weights -- nothing left to train on
train2 learnRate (img:is) (ans:as) weights =
    if percieve img == ans
        then train is as weights
        else train is as (update weights)
    where
        percieve img = percieve img weights step
        update weigths = updateWeights learnRate 1 img weights




{-
rndElem :: [a] -> IO a
rndElem xs = do rand <- randomRIO (0, length xs)
                return $ xs !! rand
-}
