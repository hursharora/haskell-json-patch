{-# LANGUAGE DeriveGeneric #-}
module TextJsonParser (
  parseJsonFromRoot,
  Value(..),
  Json(..),
  KVPair,
) where

import Data.Text as T (Text, tail, pack, head, span, dropWhile, unpack, append, concat, empty, stripStart, last, take, drop )
import Data.Char (isDigit)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- Data Definition

type Key = Text
type KVPair = (Key, Value)
data Json = Array [Value] | Object [KVPair] deriving (Show, Read, Ord, Generic)
data Value = NumD Double | NumI Int | Str Text | Boolean Bool | Json Json | Null
    deriving (Eq, Show, Read, Ord, Generic)

instance NFData Value
instance NFData Json

instance Eq Json where
  (Object kvs1) == (Object kvs2) = S.fromList kvs1 == S.fromList kvs2
  (Array _) == (Object _) = False
  (Object _) == (Array _) = False
  (Array vs1) == (Array vs2) = vs1 == vs2 -- in array order matters


-- Entry point for the parser

parseJsonFromRoot :: Text -> Json
parseJsonFromRoot t = case  T.head trimmed of
  '{' -> Object kvList
  '[' -> Array arr
  _ -> error "Invalid Json"
  where
    trimmed = T.stripStart t
    (kvList, rst) = parseKVList ([], T.stripStart $ T.tail trimmed)
    (arr, rest) = parseArray ([], T.stripStart $ T.tail trimmed)


-- Helpers: each helper parses a certain type of data eg. Array [], Object {}, etc.

parseArray :: ([Value], Text) -> ([Value], Text)
parseArray (vals, t) =
  if T.head t == ']' then ([], T.tail t) else
  case T.head rstTrimmed of
    ',' -> parseArray (newVals, T.stripStart $ T.tail rstTrimmed)
    ']' -> (newVals, T.stripStart $ T.tail rstTrimmed)
    _ -> error "parseArray error"
    where
      (v, rst) = parseValue $ T.stripStart t
      rstTrimmed = T.stripStart rst
      newVals = vals ++ [v]

isFloat = elem '.' . unpack


parseNumber :: Text -> (Value, Text)
parseNumber t = if isFloat val then (NumD (parseDouble val), rst) else (NumI (parseInteger val), rst)
  where (val, rst) = T.span (\c -> isDigit c || c == '.') t

parseDouble :: Text -> Double
parseDouble t = read $ unpack t :: Double

parseInteger :: Text -> Int
parseInteger t = read $ unpack t :: Int

parseJson :: Text -> (Json, Text)
parseJson t = case T.head $ T.stripStart t of
  '{' -> (Object kvList, rst)
  '[' -> (Array arr, rest)
  _ -> error "Invalid Json"
  where
    (kvList, rst) = parseKVList ([], T.stripStart $ T.tail t)
    (arr, rest) = parseArray ([], T.stripStart $ T.tail t)

parseKVList :: ([KVPair], Text) -> ([KVPair], Text)
parseKVList (pairs, t) = case T.head trimmed of
  '\"' -> case T.head rstTrimmed  of
    ',' -> parseKVList (newPairs, T.dropWhile (/= '\"') rst)
    '}' -> (newPairs, T.tail rstTrimmed)
    _ -> error "KVList error"
    where (p, rst) = parseKVPair trimmed
          newPairs = pairs ++ [p]
          rstTrimmed = T.stripStart rst
  '}' -> ([], T.tail t)
  _ -> error "Key must start with \""
  where trimmed = T.stripStart t

parseKVPair :: Text -> ((Key, Value), Text)
parseKVPair t = ((key, value), rest)
  where (key, ts) = parseKey t
        (value, rest) = parseValue $ T.tail $ T.stripStart ts

parseStr :: Text -> (Key, Text)
parseStr t =
  case T.head t of
  '\"' -> case T.span (/= '\"') $ T.tail t of {
      (txt, txt') -> if txt == T.empty
                      then (txt, T.tail txt')
                      else if txt' == T.empty
                      then (txt', txt')
                      else
                          if T.last txt == '\\' && T.head txt' == '\"' then (T.concat [txt, pack "\"", keyRest], restTxt)
                          else (txt, T.tail txt')
      where (keyRest, restTxt) = parseStr txt'
  }
  _ -> error "Text must be surrounded by \""

parseKey :: Text -> (Key, Text)
parseKey t = if rest == T.empty then error "rest is empty" else (key, rest)
  where (key, rest) = parseStr t

parseValue :: Text -> (Value, Text)
parseValue t =
  case T.head valueText of
  '\"' -> let (sVal, rest) = parseStr valueText in (Str sVal, rest)
  '{' -> let (jVal, rest) = parseJson valueText in (Json jVal, rest)
  '[' -> let (jVal, rest) = parseJson valueText in (Json jVal, rest)
  s | isDigit s -> parseNumber valueText
  'n' ->
    if T.take 4 valueText == pack "null"
      then (Null, T.drop 4 valueText)
      else error "invalid json, char out of quotes"
  't' ->
    if T.take 4 valueText == pack "true"
      then (Boolean True , T.drop 4 valueText)
      else error "invalid json, char out of quotes"
  'f' ->
    if T.take 5 valueText == pack "false"
      then (Boolean False, T.drop 5 valueText)
      else error "invalid json, char out of quotes"
  _ -> error "parseValue - invalid value"
  where valueText = T.stripStart t
