{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.Char (isLetter)
import Data.List (group, sort, sortOn)
import Data.Ord (Down (Down), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt (blockListF', fmt, nameF, unlinesF, (+|), (|+))
import System.Environment (getArgs)

type Entry = (Text, Int)

type Vocabulary = [Entry]

extractVocabulary :: Text -> Vocabulary
extractVocabulary t = map buildEntry $ group $ sort words
  where
    words = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
    buildEntry xs@(x : _) = (x, length xs)
    buildEntry [] = error "unexpected"
    cleanWord = T.dropAround (not . isLetter)

allWordsReport :: Vocabulary -> Text
allWordsReport vocabulary = fmt $ nameF "All words" $ unlinesF (allWords vocabulary)

allWords :: Vocabulary -> [Text]
allWords = map fst

wordsCountReport :: Vocabulary -> Text
wordsCountReport vocabulary = fmt $ "Total number of words: " +| total |+ "\nNumber of unique words: " +| unique |+ "\n"
  where
    (total, unique) = wordsCount vocabulary

wordsCount :: Vocabulary -> (Int, Int)
wordsCount vocabulary = (sum $ map snd vocabulary, length vocabulary)

frequentWordsReport :: Vocabulary -> Int -> Text
frequentWordsReport vocabulary num = fmt $ nameF "Frequent words" $ blockListF' "" fmtEntry reportData
  where
    reportData = take num $ wordsByFrequency vocabulary
    fmtEntry (t, n) = "" +| t |+ ": " +| n |+ ""

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortOn (Down . snd)

processTextFile :: FilePath -> Bool -> Int -> IO ()
processTextFile filename withAllWords n = do
  text <- TIO.readFile filename
  let vocabulary = extractVocabulary text
  when withAllWords $ TIO.putStrLn $ allWordsReport vocabulary
  TIO.putStrLn $ wordsCountReport vocabulary
  TIO.putStrLn $ frequentWordsReport vocabulary n

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-a", filename, num] ->
      processTextFile filename True (read num)
    [filename, num] ->
      processTextFile filename False (read num)
    _ ->
      putStrLn "Usage: vocab3 [-a] filename freq_words_num"
