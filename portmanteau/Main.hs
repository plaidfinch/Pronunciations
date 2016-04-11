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
import           System.Console.GetOpt

import Pronunciations
import PhoneticPortmanteau

-- TODO: Make more UNIX-y, by talking to stderr, being more silent
-- (take an --interactive flag?)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  -- Parse command line --------------------------------------------------

  -- Parse command line, getting a list of option actions and positional
  -- arguments. The 'Permute' value instructs getOpt to allow options and
  -- non-option (positional args) to be freely interspersed.
  (actions, posArgs, errors) <- getOpt Permute options <$> getArgs

  -- fold the options over the list of actions
  opts <- foldl (>>=) (return defaultOptions) actions

  -- check positional arguments
  [dictFilename, word1] <- case length posArgs of
    2 -> return posArgs
    _ -> do
      hPutStrLn stderr $ "error: you must specify both the dictionary <DICT> "
                      ++ "and the first word <WORD>"
      exitFailure

  -- extract parameters to use
  let Options { optWord1Drop    = word1Drop
              , optWord2Drop    = word2Drop
              , optOverlap      = reqOverlap
              , optMinPortAdded = minPortAdded
              } = opts


   -- Find portmanteaus ---------------------------------------------------

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
      putStrLn $ replicate 30 '-' ++ "\n"


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


-- Command Line Analysis -----------------------------------------------

data Options = Options
  { optWord1Drop    :: Int
  , optWord2Drop    :: Int
  , optOverlap      :: Int
  , optMinPortAdded :: Int
  }

-- | Default options
defaultOptions = Options
  { optWord1Drop    = 0
  , optWord2Drop    = 0
  , optOverlap      = 0
  , optMinPortAdded = 0
  }

-- | A list of actions to take based on command line options present
--
-- The 'ReqArg' constuctor specified an (optional) option that requires an
-- argument.
options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "d" ["word1drop"]
        (ReqArg
            (\arg opt -> return opt { optWord1Drop = read arg })
            "INT")
        (unwords [ "phonemes dropped from <WORD>"
                 , "\n", "[default ="
                 , show (optWord1Drop defaultOptions), "]"])

  , Option "e" ["word2drop"]
        (ReqArg
            (\arg opt -> return opt { optWord2Drop = read arg })
            "INT")
        (unwords [ "phonemes dropped from second (dictionary) word"
                 , "\n", "[default ="
                 , show (optWord2Drop defaultOptions), "]"])

  , Option "o" ["overlap"]
        (ReqArg
            (\arg opt -> return opt { optOverlap = read arg })
            "INT")
        (unwords [ "phonemes overlapped between <WORD> and second word"
                 , "\n", "[default ="
                 , show (optOverlap defaultOptions), "]"])

  , Option "m" ["min-port-added"]
        (ReqArg
            (\arg opt -> return opt { optMinPortAdded = read arg })
            "INT")
        (unwords [ "required minimum number of phonemes added to <WORD>"
                 , "in final portmanteau"
                 , "(this can be used to filter out short words)"
                 , "\n", "[default ="
                 , show (optMinPortAdded defaultOptions), "]"])

  , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                let header = prg ++ " <DICT> <WORD> [OPTIONS]"
                hPutStrLn stderr (usageInfo header options)
                exitSuccess))
        "Show help"
  ]
