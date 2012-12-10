module Main
       where

import System.Environment (getArgs)
import Data.List ()
import System.Exit (exitSuccess, exitFailure)
import GHC.IO.IOMode (IOMode(..))
import Func
       (Image,readImages, Face,step,perceiveFace, rotImgCorrect)
import Control.Monad (zipWithM_)

-- | main tries to classify the images in the file given as the first paramater
-- and output the correct face as 1,2,3 or 4.  If no file is specified it exits
-- with an error. This program is run with already trained weights saved in the
-- project root. The files loaded are "eye-weights" and "mouth-weights".

main :: IO ()
main = do
    args <- getArgs
    if null args
       then do putStrLn "No arguments given, exiting"
               exitFailure
       else do

    inputContents <- readFile $ head args
    dirtyEyeW <- readFile "/home/id09/id09bnn/edu/5DV121/lab1/eye-weights"
    dirtyMouthW <- readFile "/home/id09/id09bnn/edu/5DV121/lab1/mouth-weights"
    let images = map (rotImgCorrect 20) $ readImages inputContents
        eyew = read dirtyEyeW :: [Double]
        mouthw = read dirtyMouthW :: [Double]
        answers = classify eyew mouthw images
        output = zip answers [1..]
    -- Format the output and map putStrLn over the output.
    mapM_ (\(ans, num) -> putStrLn $ "Image" ++ (show num)
                          ++ " " ++ (show ans)) output
    exitSuccess


classify :: [Double] -> [Double] -> [[Double]] -> [Int]
classify eyew mouthw images = map (perceiveFace eyew mouthw step) images
