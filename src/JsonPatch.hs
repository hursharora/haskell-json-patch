{-# LANGUAGE OverloadedStrings #-}
module JsonPatch (
  parsePatch,
  traverseApply,
  Operation(..),
  applyPatch,
  objectToOperation
)
where

import Data.Text as T (Text, head, tail, breakOn, unpack, splitAt, dropWhile, splitOn, append, replace, length)
import Data.Char(isDigit)
import TextJsonParser (Value(Json, Str), Json(Object, Array), parseJsonFromRoot)

-- Data Definitions

data Operation = Add Text Value | Remove Text | Replace Text Value | Copy Text Text | Move Text Text | Test Text Value
  deriving (Eq, Show, Read, Ord)

-- Parse Operations from Json String
parsePatch :: Text -> [Operation]
parsePatch t = Prelude.map objectToOperation ops
  where (Array ops) = parseJsonFromRoot t

objectToOperation :: Value -> Operation
objectToOperation (Json (Object kvs))
  | op == "add" = Add path v
  | op == "remove" = Remove path
  | op == "move" = Move from path
  | op == "copy" = Copy from path
  | op == "replace" = Replace path v
  | op == "test" = Test path v
  | otherwise = error "invalid operation"
  where
    op = getOperation kvs
    path = getPath kvs
    from = getFrom kvs
    v = getValue kvs
objectToOperation _ = error "invalid Operation"

getOperation :: [KVPair] -> Text
getOperation kvs = case lookup "op" kvs of
  Nothing -> error "Operation Type must exist"
  Just (Str va) -> va
  Just _ -> "Operation must be of type string"

getPath :: [KVPair] -> Text
getPath kvs = case lookup "path" kvs of
  Nothing -> error "path must exist"
  Just (Str va) -> va
  Just _ -> "path must be of type string"

getFrom :: [KVPair] -> Text
getFrom kvs = case lookup "from" kvs of
  Nothing -> error "from must exist"
  Just (Str va) -> va
  Just _ -> "from must be of type string"

getValue :: [KVPair] -> Value
getValue kvs = case lookup "value" kvs of
  Nothing -> error "value must exist"
  Just va -> va


-- Apply an Operation to a Json object

traverseApply :: Operation -> Json -> Json
traverseApply (Add path v) json@(Object kvs)
  | path == "" = let (Json r) = v in r
  | rest == "" = addValueAtKey json key v
  | otherwise = case valueAtKey of
    Just (Json next) -> Object (prev ++ [(key, Json (traverseApply (Add rest v) next))] ++ Prelude.tail rst)
    Just _ -> error "cannot add to canonical values"
    Nothing -> error "value does not exist at path"
  where
      (key, rest) = getNextSubPath path
      valueAtKey = getValueAtKey json key
      (prev, rst) = splitAtKey kvs key

traverseApply (Add path v) json@(Array vs)
  | path == "" = let (Json r) = v in r
  | rest == "" = addValueAtKey json index v
  | otherwise = case valueAtKey of
    Just (Json next) -> Array (prev ++ [Json (traverseApply (Add rest v) next)] ++ Prelude.tail rst)
    Just _ -> error "cannot add to canonical values"
    Nothing -> error "value does not exist at path"
  where
      (index, rest) = getNextSubPath path
      valueAtKey = getValueAtKey json index
      (prev, rst) = splitAtIndex vs index

traverseApply (Remove path) json@(Object kvs)
  | rest == "" = case getValueAtKey json key of
    Just _ -> removeValueByKey json key
    Nothing -> error "removing non existant valeu"
  | otherwise = Object (prev ++ [(key, Json (traverseApply (Remove rest) next))] ++ rst)
  where
      (key, rest) = getNextSubPath path
      Just (Json next) = getValueAtKey json key
      (prev, _:rst) = splitAtKey kvs key

traverseApply (Remove path) json@(Array vs)
  | rest == "" = case getValueAtKey json index of
    Just _ -> removeValueByKey json index
    Nothing -> error "removing non existant valeu"
  | otherwise = Array (prev ++ [Json (traverseApply (Remove rest) next)] ++ rst)
  where
      (index, rest) = getNextSubPath path
      Just (Json next) = getValueAtKey json index
      (prev, _:rst) = splitAtIndex vs index

traverseApply (Replace "" (Json v)) json = v
traverseApply (Replace path v) json = traverseApply (Add path v) $ traverseApply (Remove path) json
traverseApply (Copy from path) json = case getValueAtPath json from of
  Just v -> traverseApply (Add path v) json
  Nothing -> error "copy error: value at \"from\" must exist"

traverseApply (Move from path) json
  | from == path = json
  | otherwise = case getValueAtPath json from of
    Just v -> traverseApply (Remove from) $ traverseApply (Add path v) json
    Nothing -> error "move error: value at \"from\" must exist"

traverseApply (Test path v) json@(Object kvs)
  | rest == "" = case getValueAtKey json key of
              Nothing -> error "value at path does not exist"
              Just va -> if v == va then json else error "value at path does not match"
  | null rst = error "split could not find elm with given key"
  | otherwise = Object (prev ++ [(key, Json (traverseApply (Test rest v) next))] ++ Prelude.tail rst)
  where
      (key, rest) = getNextSubPath path
      Just (Json next) = getValueAtKey json key
      (prev, rst) = splitAtKey kvs key

traverseApply (Test path v) json@(Array vs)
  | rest == "" = case getValueAtKey json key of
      Nothing -> error "value at path does not exist"
      Just va -> if v == va then json else error "value at path does not match"
  | null rst = error "split could not find elm with given key"
  | otherwise = Array (prev ++ [Json (traverseApply (Test rest v) next)] ++ Prelude.tail rst) -- value must be at head of rst
  where (key, rest) = getNextSubPath path
        Just (Json next) = getValueAtKey json key
        (prev, rst) = splitAtIndex vs key

-- Helpers

getNextSubPath :: Text -> (Text, Text)
getNextSubPath t = case T.head t of
  '/' -> (replace "~1" "/" (replace "~0" "~" curr), rest)
  _ -> error "invalid path"
  where (curr, rest) = T.breakOn "/" $ T.tail t

getValueAtKey :: Json -> Text -> Maybe Value
getValueAtKey (Object kvs) key = lookup key kvs
getValueAtKey (Array vs) "-" = case drop (Prelude.length vs - 1) vs of
  [] -> Nothing
  va : vas -> Just va

getValueAtKey (Array vs) index
  | T.length index > 1 && T.head index == '0' = error "no leading zeros in array index"
  | otherwise = case drop i vs of
  [] -> Nothing
  va : vas -> Just va
  where i = read (unpack index) :: Int


splitAtIndex :: [Value] -> Text -> ([Value], [Value])
splitAtIndex vs index
  | index == "-" = let i = Prelude.length vs in Prelude.splitAt i vs
  | readIndex index < 0 = error "out of bound index, less than zero"
  | readIndex index > Prelude.length vs = error "out of bound index, bigger than size"
  | otherwise = if T.length index > 1 && T.head index == '0'
      then error "no leading zeros in array index"
      else let i = readIndex index in Prelude.splitAt i vs


readIndex :: Text -> Int
readIndex index = read (unpack index) :: Int

getIndex :: [Value] -> Text -> Int
getIndex arr "-" = Prelude.length arr
getIndex _ index = readIndex index


addValueAtKey :: Json -> Text -> Value -> Json
addValueAtKey (Object kvs) key v = case lookup key kvs of
  Just _ -> Object ((key, v) : filter (\(k, _) -> k /= key) kvs)
  Nothing -> Object ((key, v): kvs)
addValueAtKey (Array vs) index v = Array (prev ++ [v] ++ rest)
  where (prev, rest) = splitAtIndex vs index

isIndex :: Text -> Bool
isIndex txt = T.dropWhile isDigit txt == ""

getNextValue :: Text -> Json
getNextValue path =
  if nextSub == "-" || isIndex nextSub
    then Array []
    else Object []
  where (nextSub, rst) = getNextSubPath path

type KVPair = (Text, Value)
splitAtKey :: [KVPair] -> Text -> ([KVPair], [KVPair])
splitAtKey kvs key = break (\(k, _) -> k == key) kvs


removeValueByKey :: Json -> Text -> Json
removeValueByKey (Object kvs) key = case rest of
  [] -> Object prev
  _:rst -> Object (prev ++ rst)
  where (prev, rest) = splitAtKey kvs key

removeValueByKey (Array vs) index = case rest of
  [] ->  Array prev
  _:rst -> Array (prev ++ rst)
  where (prev, rest) = splitAtIndex vs index

getValueAtPath :: Json -> Text -> Maybe Value
getValueAtPath json path
  | rest == "" = getValueAtKey json key
  | otherwise = getValueAtPath next rest
  where (key, rest) = getNextSubPath path
        Just (Json next) = getValueAtKey json key

verifyPathExists :: Json -> Text -> Bool
verifyPathExists json path = case getValueAtPath json path of
  Nothing -> False
  Just va -> True

applyPatch :: [Operation] -> Json -> Json
applyPatch [] doc = doc
applyPatch (op:rst) doc = applyPatch rst $ traverseApply op doc
