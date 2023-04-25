module Main where

import Data.Char (isLetter)
import Data.List (group, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

main :: IO ()
main = do
  [filename] <- getArgs
  text <- TIO.readFile filename

  let words = T.words text
  let words' = map (T.dropAround $ not . isLetter) words
  let cleanedWords = filter (not . T.null) words'
  let uniqueWords = map head $ group $ sort $ map T.toCaseFold cleanedWords

  TIO.putStrLn $ T.unwords uniqueWords
  print $ length uniqueWords
