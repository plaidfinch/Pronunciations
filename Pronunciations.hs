{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}

module Pronunciations
   ( homophonesUsing
   , Pronunciations
   , pronounceUsing
   , spellUsing
   , pronunciations
   , entries
   , Entry(..)
   , Word(..)
   , getWord
   , Pronunciation
   , unMark
   , Phoneme(..)
   ) where

import           Data.Attoparsec.Text hiding ( Number(..) )
import           Data.Attoparsec.Combinator
import           Data.Char
import           Data.String
import           Data.Text ( Text )
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import           Data.Set        ( Set )
import           Data.Map.Strict ( Map )
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map

import Data.Sequence ( Seq , (|>), (<|) , (><) , ViewL(..) , ViewR(..) )
import qualified Data.Sequence as Seq

import Data.Traversable
import Control.Monad hiding ( mapM )
import Control.Arrow
import Control.Applicative
import Data.Functor
import Data.Maybe
import Data.Either
import Data.List ( groupBy )

import GHC.Generics ( Generic )

import System.IO

import Prelude hiding ( takeWhile , mapM )

homophonesUsing :: Pronunciations -> Word -> Maybe (Set Word)
homophonesUsing dict =
   fmap Set.unions . mapM (spellUsing dict) . Set.toList <=< pronounceUsing dict

data Pronunciations =
   Pronunciations !(Map Word (Set Pronunciation))
                  !(Map Pronunciation (Set Word))

pronounceUsing :: Pronunciations -> Word -> Maybe (Set Pronunciation)
pronounceUsing (Pronunciations dict _) spelling = Map.lookup spelling dict

spellUsing :: Pronunciations -> Pronunciation -> Maybe (Set Word)
spellUsing (Pronunciations _ dict) saying = Map.lookup saying dict

pronunciations :: [Entry] -> Pronunciations
pronunciations =
   Pronunciations <$> (Map.fromListWith Set.union .
                       map (_word &&& Set.singleton . _pronunciation))
                  <*> (Map.fromListWith Set.union .
                       map (_pronunciation  &&& Set.singleton . _word))

entries :: Text -> Maybe [Entry]
entries = either (const Nothing) Just
        . fmap catMaybes
        . mapM (parseOnly line)
        . Text.lines

line :: Parser (Maybe Entry)
line = skipSpace *> ((";;;" *> many' (notChar '\n') $> Nothing)
                    <|> ((endOfLine <|> endOfInput) $> Nothing)
                    <|> (Just <$> entry))

data Entry = Entry { _word          :: Word
                   , _index         :: Int
                   , _pronunciation :: Pronunciation }
                   deriving (Eq, Ord, Show, Generic)

entry :: Parser Entry
entry = Entry <$> wordOrPunc
              <*> (option 0 $ "(" *> decimal <* ")")
              <*> (skipSpace *> pronunciation)

data Word = Word Text
          | Punctuation Text Text
          deriving (Eq, Ord, Show, Generic)

instance IsString Word where
   fromString = Word . Text.pack

getWord :: Word -> Text
getWord (Word word)             = word
getWord (Punctuation mark name) = mark

isWordChar :: Char -> Bool
isWordChar c = isAlpha c
            || isDigit c
            || c == '\''
            || c == '.'
            || c == '-'
            || c == '_'

-- | A word is a word if it begins with an alphabetic character or is wrapped in square brackets;
--   otherwise, it is a named piece of punctuation given by a string like "%PERCENT".
wordOrPunc :: Parser Word
wordOrPunc = (Word <$> (Text.cons <$> satisfy isAlpha
                                  <*> takeWhile isWordChar))
          <|> "[" *> (Word <$> takeWhile1 isWordChar) <* "]"
          <|> (Punctuation <$> takeWhile1 (not . isAlpha)
                           <*> takeWhile1 isWordChar)

type Pronunciation = Seq Phoneme

pronunciation :: Parser Pronunciation
pronunciation = Seq.fromList <$> phoneme `sepBy1` " "

unMark :: Phoneme -> Either Vowel Consonant
unMark (Vowel p _)   = Left p
unMark (Consonant p) = Right p

phoneme :: Parser Phoneme
phoneme =  (Vowel <$> vowel <*> decimal)
       <|> (Consonant <$> consonant)

data Phoneme = Vowel Vowel Int
             | Consonant Consonant
             deriving (Eq, Ord, Show, Generic)

vowel :: Parser Vowel
vowel =  "AA" $> AA <|> "AE" $> AE <|> "AH" $> AH
     <|> "AO" $> AO <|> "AW" $> AW <|> "AY" $> AY
     <|> "EH" $> EH <|> "ER" $> ER <|> "EY" $> EY
     <|> "IH" $> IH <|> "IY" $> IY <|> "OW" $> OW
     <|> "OY" $> OY <|> "UH" $> UH <|> "UW" $> UW

data Vowel = AA | AE | AH | AO | AW
           | AY | EH | ER | EY | IH
           | IY | OW | OY | UH | UW
           deriving (Eq, Ord, Show, Generic)

consonant :: Parser Consonant
consonant =  "B"  $> B  <|> "CH" $> CH <|> "DH" $> DH <|> "D"  $> D  <|> "F"  $> F
         <|> "G"  $> G  <|> "HH" $> HH <|> "JH" $> JH <|> "K"  $> K  <|> "L"  $> L
         <|> "M"  $> M  <|> "NG" $> NG <|> "N"  $> N  <|> "P"  $> P  <|> "R"  $> R
         <|> "SH" $> SH <|> "S"  $> S  <|> "TH" $> TH <|> "T"  $> T  <|> "V"  $> V
         <|> "W"  $> W  <|> "Y"  $> Y  <|> "ZH" $> ZH <|> "Z"  $> Z

data Consonant = B  | CH | D  | DH | F 
               | G  | HH | JH | K  | L 
               | M  | N  | NG | P  | R 
               | S  | SH | T  | TH | V 
               | W  | Y  | Z  | ZH
               deriving (Eq, Ord, Show, Generic)
