module Main
       where

import Func        (getEyesMouth, kFold, readAnswers, readImages, rotImgCorrect,
                    shuffle)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    let args =["training.txt"]
    if null args
       then do putStrLn "No arguments given, exiting"
               exitFailure
       else do

    inputContents <- readFile $ head args
    inputAnswers <- readFile "training-facit.txt"
    let images = map (rotImgCorrect 20) $ readImages inputContents
    --Writing all images to file
    --zipWithM_ (\image num -> writePGM ("rot" ++ (show num) ++ ".pgm");;;;;
    -- ;;;;; $ (rotImgCorrect image 20)) images [1..]
    --writePGM "orig.pgm" $ images !! 10
    --writePGM "rotated.pgm" $ rotImgRight (images !! 10) 20

    let
        answers = readAnswers inputAnswers
        (eyeAnswers, mouthAnswers) = unzip (map getEyesMouth answers)
        eyes = zip images eyeAnswers
        mouth = zip images mouthAnswers

    shuffled <- shuffle (zip eyes mouth)

    let (shuffledEyes, shuffledMouth) = unzip shuffled
        (_testEyes, _trainEyes) = splitAt 50 shuffledEyes
        (_testMouth, _trainMouth) = splitAt 50 shuffledMouth
--    permuEyes <- shuffle $ take 10000 $ cycle trainEyes
--    permuMouth <- shuffle $ take 10000 $ cycle trainMouth
{-
    writePGM "img1.pgm" (head images)
    eyeweights2 <- train trainEyes testEyes
    writePGM "eyew.pgm" eyeweights2
    writeFile "eye-weights" $ show eyeweights2
    putStrLn $ show eyeweights2
    mouthweights2 <- train trainMouth testMouth
    writePGM "mouthw.pgm" mouthweights2
    writeFile "mouth-weights" $ show mouthweights2
-}

    putStrLn "Eyes "

    tst <- kFold 25 eyes
    putStrLn $ "Bertil " ++ (show tst)
    putStrLn "Mouth "
    kFold 25 mouth >>= (putStrLn . show)

    exitSuccess
