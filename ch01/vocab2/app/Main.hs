module Main where

import Data.Char (isLetter)
import Data.List (group, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

type Entry = (T.Text, Int)

type Vocabulary = [Entry]

extractVocabulary :: T.Text -> Vocabulary
extractVocabulary t = map buildEntry $ group $ sort words
  where
    words = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
    buildEntry xs@(x : _) = (x, length xs)
    cleanWord = T.dropAround (not . isLetter)

printAllWords :: Vocabulary -> IO ()
printAllWords vocabulary = do
  putStrLn "All words: "
  TIO.putStrLn $ T.unlines $ map fst vocabulary

processTextFile :: FilePath -> IO ()
processTextFile filename = do
  text <- TIO.readFile filename
  let vocabulary = extractVocabulary text
  printAllWords vocabulary

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> processTextFile filename
    _ -> putStrLn "Usage: vocab-builder filename"
