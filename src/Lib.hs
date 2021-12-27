-- 

module Lib
    ( someFunc,
      Value(..),
      JsonObject(..),
      parsemain,
      parseJsonFile,
      parseJsonFileWithOutput
    ) where

import Data.Char (isSpace)
import Data.List
import Text.Read
import Data.Bool (otherwise)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-
    This function takes user input for a file Name, then reads that file and returns the parsed JsonObject in Haskell
-}

parseJsonFile :: IO JsonObject
parseJsonFile = putStrLn "Enter File:" >> getLine >>= readFile >>= (return . parsemain)

{-
    This function takes user input for a file Name, then reads that file and writes the parsed JsonObject in Haskell into the file Output.txt
-}

parseJsonFileWithOutput :: IO ()
parseJsonFileWithOutput = putStrLn "Enter File:" >> getLine >>= readFile >>= (return . parsemain) >>= writeFile "Output.txt" . show >> putStrLn "Done"

todo = undefined

-- A simple Value consisting of either an Integer, a String, a Double or a Maybe Boolean, with Nothing representing Null
data Value = IVal Int | SVal String | DVal Double | BVal (Maybe Bool)
    deriving (Eq, Show, Read, Ord)

-- A JsonObject consisting of a key, and either it's associated value or a list of JsonObjects, if it's an object.
-- A "" key value means that there is no key value
data JsonObject = JsonObject String (Either Value [JsonObject])
    deriving (Eq, Show, Read)

{-
    Things to work on in the future.
    Brittleness : The string "{\"name\":\"John\", \"age\":30, \"car\": null}" works, but the string "'{\"name\":\"John\", \"age\":30, \"car\": null}'" does
                    not work because of the extra stuff at the start. Although it probably should throw an error anyways in that case because that's not proper Json
                    convention.

-}

{-
    @parsemain s@ takes the valid JSON string @s@ and parses it to create a JsonObject.
    Creates a JsonObject with no key, and a list of children created by calling parseSections True.
-}
parsemain :: String -> JsonObject
parsemain s = JsonObject "" (Right (parseSections True (dropFirstLast s)))

{-
    @parseSections ky s@
    ky stands for keyYes, and distinguishes between whether to check for keys (regular object parsing) or not (array parsing)
    If ky == True:
    Trims off whitespace from front and back and then drops the first and last characters, which should be a pair of {} brackets
    Checks if key-value delimiter ':" exists. If it does, calls parseSection with the key value before the ':', and the value after the delimiter
    If it doesn't exist, then return a []. This is the base case.

    If ky == False:
    Calls parseSection on entire String with key "". Used for array items.

-}
parseSections :: Bool -> String -> [JsonObject]
parseSections True s = let workStr = trimWhitespace s
                        in case workStr of
                            ('\"':rest) -> case getPairedQuote rest of
                                            (Just a) -> parseSection True (fetchString (take a workStr))  ((trimWhitespace . snd . strBreak ':') (drop a workStr))
                                            Nothing -> error "Invalid Json"
                            otherwise -> []
parseSections False s = let workStr = trimWhitespace s
                        in if workStr /= ""
                            then parseSection False "" workStr
                            else []


{- 
    @parseSection keyYes key string@
    Checks the first char of string to figure out what type of item it is. Calls the proper function on everything up to the next item delimiter ',".
        Then recursively calls parseSections on the remaining String, which should contain the rest of the items.
    If \", then it's a String, which means either a Boolean value, or an actual String, so we call parseString on 
        everything up to the next item delimiter ",". If "," doesn't exist, then just parseString the entire string.
    If [, then it's an Array, so we call parseArray on everything up to the matching ] bracket, then recursively call parseSections on the rest of the string excluding the ',' delimiter.
        If there is no matching ] bracket, errors out.
    If {, then it's an Object,so we call parseObject on everything up to the matchign } bracket, then recursively call parseSections on the rest of the string excluding the ',' delimiter.
        If there is no matching } bracket, errors out.
    If it does not match any of these cases, then it must be a value, so we call parseValue on everything up to the next "," delimiter.
        If there is no next item, then just call parseValue on the entire string.

    Passes the keyYes value onto it's recursive calls (important for arrays to work)
-}
parseSection :: Bool -> String -> String -> [JsonObject]
parseSection ky k "" = []
parseSection ky k s@('\"':rest) = case getPairedQuote rest of
                                    (Just a) -> case elemIndex ',' (drop a s) of
                                                (Just b) -> parseString k (take a s) : parseSections ky (drop (b + 1) (drop a s))
                                                Nothing -> [parseString k (take a s)]
                                    Nothing -> error "Parsing Error, Mismatched Quotes"
parseSection ky k s@('[':rest) = case getPairedIndex '[' ']' rest of
                                    (Just a) -> case elemIndex ',' (drop a s) of
                                                    (Just b) -> parseArray k (take a s) : parseSections ky (drop (b + 1) (drop a s))
                                                    Nothing -> [parseArray k (take a s)]
                                    Nothing -> error "Parsing Error, Mismatched Square Brackets"
parseSection ky k s@('{':rest) =  case getPairedIndex '{' '}' rest of
                                    (Just a) -> case elemIndex ',' (drop a s) of 
                                                    (Just b) -> parseObject k (take a s) : parseSections ky (drop (b + 1) (drop a s))
                                                    Nothing -> [parseObject k (take a s)]
                                    Nothing -> error "Parsing Error, Mismatched Curly Brackets"
parseSection ky k s = case elemIndex ',' s of
                        (Just a) -> parseValue k (trimWhitespace (take a s)) : parseSections ky (drop (a + 1) s)
                        Nothing -> [parseValue k (trimWhitespace s)]

{-
    @parseString k s@
    Constructs a JsonObject with key k and Value String s
-}
parseString :: String -> String -> JsonObject
parseString k s = JsonObject k (Left (SVal (fetchString s)))

{-
    @parseObject k s@
    Constructs a JsonObject with key k and a list of JsonObjects created by calling parseSections
-}
parseObject :: String -> String -> JsonObject
parseObject k s = JsonObject k (Right (parseSections True (dropFirstLast s)))


{-
    @parseValue k s@
    Constructs a JsonObject with key k and a Value that is either a null (represented by Nothing), a Boolean (true, false), a Double, or an Int.
    Whether the included number is a Double or an Int is determined by the presence of a decimal.
-}
parseValue :: String -> String -> JsonObject
parseValue k "true" = JsonObject k (Left (BVal (Just True)))
parseValue k "false" = JsonObject k (Left (BVal (Just False)))
parseValue k "null" = JsonObject k (Left (BVal Nothing))
parseValue k s = if elem '.' s
                then case (readMaybe (trimWhitespace s) :: Maybe Double) of
                        (Just a) -> JsonObject k (Left (DVal a))
                        Nothing -> error ("Value Parsing Error, Not an Int or Double : " ++ (trimWhitespace s))
                else case (readMaybe (trimWhitespace s) :: Maybe Int) of
                        (Just a) -> JsonObject k (Left (IVal a))
                        Nothing -> error ("Value Parsing Error, Not an Int or Double: " ++ (trimWhitespace s))


{-
    @parseArray k s@
    Constructs a JsonObject with key k and a list of JsonObjects created by calling parseSections False on the items in the array
-}
parseArray :: String -> String -> JsonObject
parseArray k s = JsonObject k (Right (parseSections False (dropFirstLast s)))


-- Fetchs a string by trimming all the whitespace before and after, then dropping the first and last chars, which should be the extra \" characters.
fetchString :: String -> String
fetchString = dropFirstLast . trimWhitespace

-- Trims whitespace from front
trimFront :: String -> String
trimFront = dropWhile isWhitespace

-- Trims whitespace from back
trimBack :: String -> String
trimBack = reverse . dropWhile isWhitespace . reverse

-- Trims whitespace from both front and back
-- TODO: I'm pretty sure we need to trim line break characters as well, at some point
trimWhitespace :: String -> String
trimWhitespace = trimBack . trimFront

-- Checks if Char c is whitespace, defined as either a space ' ', a carriage return '\r', a newline '\n' or a horizontal tab '\t'
isWhitespace :: Char -> Bool
isWhitespace c = isSpace c || (== '\n') c || (== '\r') c || (== '\t') c

{-
    Calls findPair with the correct inital elements. Only call this after splitting off the head bracket, or else it will not work.
    Returns Nothing if no paired bracket is found, or the index of the paired bracket.
-}

getPairedIndex :: Char -> Char -> String -> Maybe Int
getPairedIndex c1 c2 s = findPair c1 c2 1 1 s


getPairedQuote :: String -> Maybe Int
getPairedQuote s = findPair '\"' '\"' 1 1 s -- This works because findPair checks if it's equal to c2 first before checking c1

{-
    @findPair c1 c2 c1n c2id s@
    Find the index of the opposing bracket.
    Walks through string s, adding 1 to c1n if we encounter c1, subtracting 1 from c1 if we encounter c2. 
    Keep track of how many characters we've been through in c2id. Once we reach 0 in c1n, that means we've found a pair for the bracket,
    so we return c2id as that will now be the index of that paired bracket.
-}
findPair :: Char -> Char -> Int -> Int -> String -> Maybe Int
findPair c1 c2 0 c2id s = Just c2id
findPair c1 c2 c1n c2id [] = Nothing
findPair c1 c2 c1n c2id s
  | c2 == head s = findPair c1 c2 (c1n - 1) (c2id + 1) (tail s)
  | c1 == head s = findPair c1 c2 (c1n + 1)  (c2id + 1) (tail s)
  | otherwise = findPair c1 c2 c1n (c2id + 1) (tail s)


test2 = getPairedIndex '{' '}' "test"
test3 = getPairedIndex '{' '}' "te}st"
test4 = getPairedIndex '{' '}' "teeeeeeeeeee,eeeeeeeeeeeeeeeeeee eeeeeee\"e}st"


{-
    @dropFirstLast s@
    Drops the first and last characters of string s. Usually some sort of brackets or quotes.
-}
dropFirstLast :: String -> String
dropFirstLast s = (reverse . tail . reverse . tail) s

{-
    @strBreak c s@
    Break s along the given character.
    Returns (s, []) if the character doesn't exist. 
    
-}
strBreak :: Char -> String -> (String, String)
strBreak c s =  (takeWhile (not . (== c)) s , drop 1 (dropWhile (not . (== c)) s))




parsetest1 = parsemain "{\"name\":\"John\"}"
parsetest2 = parsemain "{\"name\":\"John\", \"age\":30, \"car\":null}"
parsetest3 = parsemain "{\"employees\":[\"John\", \"Anna\", \"Peter\"]}"
parsetest4 = parsemain "{\"name\":\"John\", \"age\":30, \"employees\":[\"John\", \"Anna\", \"Peter\"]}"
parsetest5 = parsemain "{\"numbers\": [[1,2], [3,4], [5,6]]}"
parsetest6 = parsemain "{\"objects\": [{\"name\":\"John\"}, {\"age\":30, \"car\":null}, {\"test\": 13}]}"
parsetest7 = parsemain "{\"objects\": [{\"name\":\"John\"}, {\"age\":30, \"car\":null}, {\"test\": 13}]}"
parsetest8 = parsemain "{\r\n\"name\":\"John\",\r\n\"age\":30,\r\n\"cars\":[\"Ford\", \"BMW\", \"Fiat\"]\r\n}"