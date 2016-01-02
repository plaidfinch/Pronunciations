{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Loops
import           Data.Functor
import           Data.List
import qualified Data.Set as Set
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Prelude hiding (Word)
import           System.IO
import           System.Environment
import           System.Exit
import           System.Directory

import Pronunciations
import PhoneticPortmanteau

-- TODO: Make more UNIX-y, by talking to stderr, being more silent (take an --interactive flag?)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  (dictFilename,word1,word1Drop,word2Drop,reqOverlap,minPortAdded) <- do
    args <- getArgs
    case args of
      [a,b,c,d,e,f] -> return (a, b, read c, read d, read e, read f)
      _             -> putStrLn "usage: dictFilename word1 word1Drop word2Drop reqOverlap minPortAdded" >> exitFailure

  dict <- getPronunciations dictFilename
  let -- List of lists because we may have multiple different pronunciations
      -- of word1.
      maybeCandidates = findCandidates dict word1 word1Drop word2Drop reqOverlap minPortAdded
  case maybeCandidates of
    Nothing -> do
      putStrLn $ "`" ++ word1 ++ "` is not in the dictionary :("
      exitFailure
    Just candidates -> forM_ (zip [0..] candidates) $ \(idx,(pronounce,group)) -> do
      putStrLn (replicate 30 '-')
      putStrLn $ "Group " ++ show idx
      putStrLn $ "Pronunciation: " ++ pronounce
      putStrLn (replicate 4 '-')
      mapM_ (T.putStrLn . T.toLower . getWord) $ Set.toList group
      putStrLn $ (replicate 30 '-') ++ "\n"


getPronunciations :: String -> IO Pronunciations
getPronunciations dictFilename = do
  fileExists <- doesFileExist dictFilename
  unless fileExists $ do
    putStrLn $ "Dictionary file does not exist: " ++ dictFilename
    exitFailure
  putStrLn "Reading dictionary file ..."
  maybeDict <- entries <$> T.readFile dictFilename
  case maybeDict of
    Nothing -> do
      putStrLn $ "Failed to parse dictionary file: " ++ dictFilename
      exitFailure
    Just dictEntries -> return (pronunciations dictEntries)
