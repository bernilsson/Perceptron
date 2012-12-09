-----------------------------------------------------------------------------
--
-- Module      :  Func
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Func (

    chunks,
    getEyesMouth,
    getFace,
    Face,
    Image,
    train,
    numCorrect,
    test,
    percieve,
    percieveFace,
    step,
    writePGM,
    shuffle,
    rotImgCorrect,
    readImages,
    readAnswers,
    kFold

) where


import System.Random (randomRIO)
import Data.Array.IO
import Data.List (sortBy, intersperse)
import Control.Monad
import Debug.Trace (trace)
import Data.Char (isAlpha)

type Image = [Double]
type Weight = Double
type Face = Int     -- 1 = Happy, 2 = Sad, 3 = Mischievous, 4 = Mad
type Eyes = Double  -- \ / up    brows = 1  / \ down brows = 0
type Mouth = Double -- \_/ happy mouth = 1  /-\ sad mouth  = 0


-------------------------------------------------------------------
-------- File read and write functions ----------------------------
-------------------------------------------------------------------

-- | readImages reads the contents of the input file and returns a list of
-- images (represented as a list of Doubles)
readImages :: String -> [Image]
readImages input =
    -- Cut input into lines, filter out empty lines, comments and image titles.
    -- Cut the lines into words and convert them to numbers.
    -- Convert the values to doubles and chunk up in separate images.
    let commentsEtc l = not(null l || head l == '#' || isAlpha (head l))
        processed = map read $ concatMap words $ tail $
                    filter commentsEtc $ lines input
        (width:height:images) = processed :: [Double]
    in  chunks (round (width * height)) (map (/31) images)


readAnswers :: String -> [Face]
readAnswers inputAnswers =
    map read $ filter (\line -> not (isAlpha (head line)))
    $ concatMap words $ removeComments (lines inputAnswers)
    where
      removeComments xs =
        filter (\line -> not (null line || (head line) == '#')) xs

-- | writePGM converts a image (or weight list) to pgm-format and writes it to
-- the given filename
writePGM :: FilePath -> [Double] -> IO ()
writePGM filename image =
  let ints = map (round . (1000*)) image :: [Int]
      posInts = map (subtract $ minimum ints) ints
      pgm = concat $ intersperse " " $ map show posInts
      header = "P2\n20 20\n" ++ show (maximum posInts) ++ "\n"
  in writeFile filename (header ++ pgm ++ "\n")

-------------------------------------------------------------------
-------- Image functions ------------------------------------------
-------------------------------------------------------------------

-- | rotImgCorrect tries to rotate the image so that the eyes are up and the
-- mouth is down.
rotImgCorrect :: Int -> Image -> Image
rotImgCorrect width img =
  let images = take 4 (iterate (rotate90 width) img)
      sumHalf = sum . filter (>0.8) . take ((width^2) `div` 2)
  in head $ sortBy (\img1 img2 -> compare (sumHalf img1) (sumHalf img2)) images

-- | rotate90 rotates a square image 90 degrees counterclockwise.
rotate90 :: Int -> [a] -> [a]
rotate90 width img = rot (chunks width img) []
    where rot :: [[a]] -> [a] -> [a]
          rot im out = if null $ head im
                           then out
                           else rot (map tail im) $ (map head im) ++ out

-------------------------------------------------------------------
-------- Training -------------------------------------------------
-------------------------------------------------------------------

learnRate = 0.0001

-- | randomWeights returns a list of n random weights between 0 and 0.1
randomWeights :: Int -> [IO Weight]
randomWeights n | n < 1 = []
                | otherwise = randomRIO (0,0.1) : randomWeights (n-1)

-- | updateWeights is used by batchTrain to modify the weights. This is
-- basically the the perceptron learning rule in action.
updateWeights :: Double -> Double -> [Double] -> [Double] -> [Double]
updateWeights learnRate error inputs weights =
    zipWith (\x y -> y + learnRate * error * x) inputs weights


-- | train takes one list of image and answer pairs to train on and one list to
-- test on.  train returns trained weights
train :: [(Image,Double)] -> [(Image,Double)] -> IO [Double]
train trainData tests = do weights <- sequence $ randomWeights numPixels
                           trainHelper trainData tests weights (0,[])
                        where numPixels = length $ fst $ head trainData

-- | trainHelper takes one list of image and answer pairs to train on, one to
-- test on, a list of weights and a tuple. The tuple should consist of the best
-- weights known yet, and how many of the images in the test list that was
-- correctly classified with those weights. train returns trained
trainHelper :: [(Image,Double)] -> [(Image,Double)] ->  [Weight]
               -> (Int, [Double]) -> IO [Double]
trainHelper xs tests weights best =
    let (newWeights, errs) = batchTrain xs weights learnRate 0
        numCorTests = numCorrect tests newWeights
        -- traceMsg = "corr " ++ (show $ numCorTests) ++ " errs " ++ (show errs)
        -- ++ " best " ++ (show $ fst best)
        current = (numCorTests, newWeights)
        newBest = if fst best < fst current then current else best
    in if numCorTests >= length tests || errs < 7
          then do return $ snd newBest
          else do shuffled <- shuffle xs
                  trainHelper shuffled tests newWeights newBest

-- | batchTrain takes a list of image and answer pairs and the weights to be
-- trained.  It returns the trained weights.
batchTrain :: [(Image,Double)] -> [Double] -> Double -> Int -> ([Double],Int)
batchTrain [] weights _ numErr = (weights, numErr) -- nothing left to train on
batchTrain ((img,ans):xs) weights learnRate numErr =
    let error = ans - percieve img weights sigmoid
        newWeights = updateWeights learnRate error img weights
    in batchTrain xs newWeights learnRate
       (if abs error > 0.5 then numErr+1 else numErr)

-------------------------------------------------------------------
-------- Perception -----------------------------------------------
-------------------------------------------------------------------

sigmoid :: Double -> Double
sigmoid x = 1/(1+(exp ( 0-x )))

step :: Double -> Double
step n = if n > 0.5 then 1 else 0

-- | percieveFace tries to classify a given image with a set of pretrained
-- weights.  returns the face as an Int according to specs.
percieveFace :: [Weight] -> [Weight] -> (Double -> Double) -> Image -> Face
percieveFace eyesWeigths mouthWeights actfn img =
    getFace (percieve img eyesWeigths actfn) (percieve img mouthWeights actfn)

-- | percieve takes a image, weights and and a activation function, and returns
-- the neurons output
percieve :: Image -> [Weight] -> (Double -> Double) -> Double
percieve inputs weights actfn = actfn $ sum $ zipWith (*) inputs weights


-- | getEyesMouth returns the state of the eyes and mouth, given the face type.
getEyesMouth :: Face -> (Eyes,Mouth)
getEyesMouth 1 = (0,1)  -- Happy
getEyesMouth 2 = (0,0) -- Sad
getEyesMouth 3 = (1,0)  -- Mischievous
getEyesMouth 4 = (1,1)   -- Mad

-- | getFace takes the state of the eyes and mouth, and returns the facetype
getFace :: Eyes -> Mouth -> Face
getFace 0 1 = 1 -- Happy
getFace 0 0 = 2 -- Sad
getFace 1 0 = 3 -- Mischievous
getFace 1 1 = 4 -- Mad
getFace n m = -9000000 -- should not happen

-------------------------------------------------------------------
-------- Testing --------------------------------------------------
-------------------------------------------------------------------

-- | kFold does kFold validation on the given input. It returns the accuracy as
-- a value between 0.0 and 1.0.
kFold :: Int -> [(Image, Double)] -> IO Double
kFold k input = helper (cycle input) times 0
    where times = length input `div` k
          helper :: [(Image, Double)] -> Int -> Int -> IO Double
          helper xs 0 totCorr = return $ fromIntegral totCorr /
                                (fromIntegral $ length input)
          helper xs n totCorr =
              let (tests, rest) = splitAt k xs
                  trainData = take (length input - k) rest
              in do weights <- train trainData tests
                    let newTot = totCorr + numCorrect tests weights
                    putStrLn $ "numCorrect " ++ show newTot
                      ++ " rounds left " ++ show n
                    helper rest (n-1) newTot


numCorrect :: [(Image, Double)] -> [Weight] -> Int
numCorrect xs weights =
  sum $ map (\(img,ans) -> fromEnum $ percieve img weights step == ans) xs

test :: [(Image, Eyes)] -> [(Image, Mouth)] -> [Weight] -> [Weight]
        -> [(Face,Face,Bool)] -> ([(Face,Face,Bool)], Int)
test [] _ _ _ xs = (xs,length $ (filter (\(_,_,tupple) -> tupple)) xs)
test ((image,eye):eyes) ((_, mouth):mouths) eyew mouthw xs =
    let aj = (percieve image eyew step)
        moth = (percieve image mouthw step)
        correct = aj == eye && moth == mouth
        in test eyes mouths eyew mouthw
           (((getFace aj moth),(getFace eye mouth),correct):xs)

-------------------------------------------------------------------
-------- Helper functions -----------------------------------------
-------------------------------------------------------------------

-- | chunks splits the list xs into chunks with length n (the last chunk may be
-- shorter).
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = y1 : chunks n y2
    where (y1, y2) = splitAt n xs

----------------------------------------------------------
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



{-
updateWeights :: Double -> Double -> [Double] -> [Double] -> [Double]
updateWeights learnRate error inputs weights =
    let h = sum $ zipWith (*) inputs weights
    in zipWith (\x y -> y + learnRate * error * x * (sigDeriv h) ) inputs weights

sigDeriv :: Double -> Double
sigDeriv x = exp x / ((1+exp x)^2)
-}
