module Beautifier (
    unParse,
    unParseNoBeautify
)
where

import TextJsonParser
import Data.Text as T (Text, unpack, pack, replace)
import Data.Char (toLower)

-- For all functions below, ind stands for indent and is the indent character used (usually either nothing, 2 space, 4 spaces or a tab character \t)
-- indlv stands for indent level, and is the level to indent for the given value/object/key-value pair being unparsed.

unParse :: String -> Json -> String
unParse ind = unParseObject ind 0

-- Calls unParse with no indent character, and removes all newline characters afterwards, resulting in a single line, non-Beautified output.
unParseNoBeautify :: Json -> String
unParseNoBeautify = removeNewlines . unParse ""

unParseObject :: String -> Int -> Json -> String
unParseObject ind indlv (Object []) = "\n" ++ ((concat . replicate indlv) ind) ++ "{ }"
unParseObject ind indlv (Object kvs) = "\n" ++ ((concat . replicate indlv) ind) ++ "{\n" ++ unParseKVs ind (indlv + 1) kvs ++ "\n" ++ ((concat . replicate indlv) ind) ++ "}"
unParseObject ind indlv (Array items) = "[" ++  unParseArray ind indlv items ++ "]"

unParseKVs :: String -> Int -> [KVPair] -> String
unParseKVs ind indlv (kv:[]) = unParseKVPair ind indlv kv
unParseKVs ind indlv (kv:rest) = unParseKVPair ind indlv kv ++ ",\n" ++ unParseKVs ind indlv rest

unParseKVPair :: String -> Int -> KVPair -> String
unParseKVPair ind indlv (key, val) =  ((concat . replicate indlv) ind) ++ (show $ T.unpack key) ++ " : " ++ unParseValue ind indlv val

unParseArray :: String -> Int -> [Value] -> String
unParseArray ind indlv [] = " "
unParseArray ind indlv (v:[]) = unParseValue ind indlv v
unParseArray ind indlv (v:rest) = unParseValue ind indlv v ++ ", " ++ unParseArray ind indlv rest

unParseValue :: String -> Int -> Value -> String
unParseValue ind _ Null = "null"
unParseValue ind _ (Str t) = (show $ T.unpack t)
unParseValue ind indlv (Json j) = unParseObject ind (indlv + 1) j
unParseValue ind _ (NumD d) = show d
unParseValue ind _ (NumI i) = show i
unParseValue ind _ (Boolean b) = (map toLower) $ show b


-- Removes all newlines in a String. Needed for the no Beautify option
removeNewlines :: String -> String
removeNewlines [] = ""
removeNewlines ('\n':xs) =  removeNewlines xs
removeNewlines (x:xs) = x : (removeNewlines xs)

-- Test Functions
test1 = unParse "\t" . parseJsonFromRoot $ T.pack "{\"foo\": 1}"
test2 = unParse "\t" . parseJsonFromRoot $ T.pack "{\"numbers\": [[1,2], [3,4], [5,6]]}"
test3 = unParse "\t" . parseJsonFromRoot $ T.pack "{\"clients\": [\n{\"name\":\"John\", \"age\":30, \"car\": null},\n {\"name\":\"Jane\", \"age\":27.3, \"car\": true},\n {\"name\":\"Steve\", \"age\":32, \"car\": false}]}"
test4 = unParse "\t" . parseJsonFromRoot $ T.pack "{\"cars\": [\"Ford\", \"BMW\", \"Fiat\"]}"

testReparse = parseJsonFromRoot . pack . unParse "\t" . parseJsonFromRoot $ T.pack "{\"clients\": [\n{\"name\":\"John\", \"age\":30, \"car\": null},\n {\"name\":\"Jane\", \"age\":27.3, \"car\": true},\n {\"name\":\"Steve\", \"age\":32, \"car\": false}]}"
testNoBeautify = writeFile "Output.txt" . unParseNoBeautify . parseJsonFromRoot $ T.pack "{\"clients\": [\n{\"name\":\"John\", \"age\":30, \"car\": null},\n {\"name\":\"Jane\", \"age\":27.3, \"car\": true},\n {\"name\":\"Steve\", \"age\":32, \"car\": false}]}"
testReparseNoBeautify = parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot $ T.pack "{\"clients\": [\n{\"name\":\"John\", \"age\":30, \"car\": null},\n {\"name\":\"Jane\", \"age\":27.3, \"car\": true},\n {\"name\":\"Steve\", \"age\":32, \"car\": false}]}"

t1 = "{\"foo\": 1}"
t2 = "{\"numbers\": [[1,2], [3,4], [5,6]]}"
t3 = "{\"clients\": [\n{\"name\":\"John\", \"age\":30, \"car\": null},\n {\"name\":\"Jane\", \"age\":27.3, \"car\": true},\n {\"name\":\"Steve\", \"age\":32, \"car\": false}]}"
t4 = "{\"cars\": [\"Ford\", \"BMW\", \"Fiat\"]}"
t5 = "{\"name\":\"John\", \"age\":30, \"car\": null}"
ts = [t1, t2, t3, t4, t5]

makeSample t = "--------------------\n\n" ++ t ++ "\n\n to \n\n" ++ (unParse "  " . parseJsonFromRoot $ T.pack t) ++ "\n\n"
writeSamples = writeFile "Output.txt" (concatMap makeSample ts)
