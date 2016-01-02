module PhoneticPortmanteau where

import           Data.Char (toLower)
import           Data.List (tails,sortBy)
import           Data.Maybe (catMaybes)
import           Data.Functor
import           Data.Function (on)
import           Data.Foldable (forM_,toList)
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import           Data.Sequence ( Seq , (|>), (<|) , (><) , ViewL(..) , ViewR(..) )
import qualified Data.Sequence as Seq
import           Data.Set        ( Set )
import qualified Data.Set        as Set
import qualified Data.Text    as T
import           Prelude hiding (Word)
import           System.Environment (getArgs)

import Pronunciations

import Debug.Trace

findCandidates :: Pronunciations
               -> String
               -> Int
               -> Int
               -> Int
               -> Int
               -> Maybe [(String, Set Word)]
findCandidates dict word1 word1Drop word2Drop reqOverlap minPortAdded = do
  let text = T.toUpper (T.pack word1)
  word1Ps <- pronounceUsing dict (Word text)
  return $ for (Set.toList word1Ps) $ \word1P ->
    let len = Seq.length word1P
        overlap =  Seq.take reqOverlap (Seq.drop (len - (reqOverlap + word1Drop)) word1P)
        pToWAssocs = Map.assocs (pronunciationToWordsMap dict)
        wordSets :: [Set Word]
        wordSets = for pToWAssocs $ \(word2P,word2Spellings) ->
          let droppedWord2P = Seq.drop word2Drop word2P
              isMatch = (Seq.length droppedWord2P - reqOverlap >= minPortAdded)
                     && (Seq.take reqOverlap droppedWord2P == overlap)
           in if isMatch
                 then word2Spellings
                 else Set.empty

     in (show (toList word1P), Set.unions wordSets)


portInfix :: String -> Int -> Int -> [String] -> [String]
portInfix seed nTails len ws = if len > seedLen
                           then error "len is too long!"
                           else catMaybes ps
  where
    seedLen = length seed
    (prefix,suffix) = splitAt (seedLen-len) seed
    matches w = if (take len w) == suffix
                   then Just $ prefix ++ w
                   else Nothing
    ps = map matches (concatMap (nTailsOfMinLen nTails len) ws)


nTailsOfMinLen :: Int -> Int -> [a] -> [[a]]
nTailsOfMinLen n len xs = take (n `min` (xsLen - len + 1)) (tails xs)
  where
    xsLen = length xs

for :: [a] -> (a -> b) -> [b]
for = flip map

-- main :: IO ()
-- main = do
--   [word,len,minPortAdded,nTails] <- getArgs
--   prts <- portInfix word (read nTails) (read len) <$> englishWords
--   let isMinLen xs = (length word + (read minPortAdded)) < length xs
--       clean = sortBy (compare `on` length) . dedup . filter isMinLen
--   mapM_ putStrLn (clean prts)
