{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pronunciations
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import System.IO
import System.Environment
import System.Directory
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List
import Control.Monad.Loops
import qualified Data.Set as Set

-- TODO: Make more UNIX-y, by talking to stderr, being more silent (take an --interactive flag?)

main :: IO ()
main = do
   hSetBuffering stdout NoBuffering
   args <- getArgs
   case args of
      dictFileName : [] -> do
         fileExists <- doesFileExist dictFileName
         if fileExists
            then do
               putStrLn "Reading dictionary file ..."
               maybeDict <- entries <$> T.readFile dictFileName
               case maybeDict of
                  Just dictEntries ->
                     let !dict = pronunciations dictEntries
                     in do putStr "> "
                           whileM_ (not <$> isEOF) $ do
                              text <- T.toUpper . T.pack <$> getLine
                              case homophonesUsing dict (Word text) of
                                 Nothing -> return ()
                                 Just results ->
                                    mapM_ (T.putStrLn . T.toLower . getWord) $ Set.toList results
                              putStr "> "
                  Nothing -> putStrLn $ "Failed to parse dictionary file: " ++ dictFileName
            else putStrLn $ "Dictionary file does not exist: " ++ dictFileName
      [] -> putStrLn "Too few arguments; please specify a single dictionary file"
      _  -> putStrLn "Too many arguments; please specify a single dictionary file"
