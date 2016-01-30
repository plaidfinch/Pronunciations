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

for :: [a] -> (a -> b) -> [b]
for = flip map

