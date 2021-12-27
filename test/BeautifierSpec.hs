{-# LANGUAGE OverloadedStrings #-}
module BeautifierSpec(
    beautifierTests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Beautifier (unParse, unParseNoBeautify)
import Data.Text (unpack, pack)
import TextJsonParser (parseJsonFromRoot, Json(Object, Array), Value(Str, NumI, Null, Boolean, NumD, Json))

-- Uses the same tests as the Parser, with each test parsing, unparsing and parsing again to ensure the data is preserved. 
-- Does not test for readability or visual aesthetic because that's somewhat subjective and hard to do with these types of tests.
beautifierTests :: TestTree
beautifierTests = testGroup "Beautifier Tests"
    [ testCase "Simple Parse/UnParse/Parse: Single String" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{\"name\": \"John\"}" @?= Object [("name", Str "John")]
    , testCase "Simple Parse/UnParse/Parse: Single Int" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{\"int\": 17}" @?= Object [("int", NumI 17)]
    , testCase "Simple Parse/UnParse/Parse: Single Double" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{\"double\": 17.3}" @?= Object [("double", NumD 17.3)]
    , testCase "Simple Parse/UnParse/Parse: Single Bool" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{\"Bool\": true}" @?= Object [("Bool", Boolean True)]
    , testCase "Simple Parse/UnParse/Parse: Single Null" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{\"n\": null}" @?= Object [("n", Null)]
    , testCase "Simple Parse/UnParse/Parse: Multiple Items" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{\"name\":\"John\", \"age\":30, \"car\": null}"
            @?= Object [("name", Str "John"), ("age", NumI 30), ("car", Null)]
    , testCase "Simple Parse/UnParse/Parse: Single Array" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{\"cars\": [\"Ford\", \"BMW\", \"Fiat\"]}"
            @?= Object [("cars", Json (Array [Str "Ford", Str "BMW", Str "Fiat"]))]
    , testCase "Complex Parse/UnParse/Parse: Multiple Levels" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{\"test\": {\"name\":\"John\", \"age\":30, \"car\": null}}"
            @?= Object [("test", Json (Object [("name", Str "John"), ("age", NumI 30), ("car", Null)]))]
    , testCase "Complex Parse/UnParse/Parse: Multiple Levels, Multiple Objects" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{\"test\": {\"name\":\"John\", \"age\":30, \"car\": null}, \"test2\" : {\"name\":\"Jane\", \"age\":27.3, \"car\": true}}"
            @?= Object [
              ("test", Json (Object [("name", Str "John"), ("age", NumI 30), ("car", Null)])),
              ("test2", Json (Object [("name", Str "Jane"), ("age", NumD 27.3), ("car", Boolean True)]))
            ]
    , testCase "Complex Parse/UnParse/Parse: Array of Arrays" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{\"numbers\": [[1,2], [3,4], [5,6]]}"
            @?= Object [("numbers", Json (Array [Json (Array [NumI 1, NumI 2]), Json (Array [NumI 3, NumI 4]), Json (Array [NumI 5, NumI 6])]))]
    , testCase "Complex Parse/UnParse/Parse: Array of Objects" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{\"clients\": [{\"name\":\"John\", \"age\":30, \"car\": null}, {\"name\":\"Jane\", \"age\":27.3, \"car\": true}, {\"name\":\"Steve\", \"age\":32, \"car\": false}]}"
            @?= Object [("clients", Json (Array [
                Json (Object [("name", Str "John"), ("age", NumI 30), ("car", Null)]),
                Json (Object [("name", Str "Jane"), ("age", NumD 27.3), ("car", Boolean True)]),
                Json (Object [("name", Str "Steve"), ("age", NumI 32), ("car", Boolean False)])
              ]))]
    , testCase "Complex Parse/UnParse/Parse: Array of Objects with Newlines" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{\"clients\": [\n{\"name\":\"John\", \"age\":30, \"car\": null},\n {\"name\":\"Jane\", \"age\":27.3, \"car\": true},\n {\"name\":\"Steve\", \"age\":32, \"car\": false}]}"
            @?= Object [("clients", Json (Array [
                Json (Object [("name", Str "John"), ("age", NumI 30), ("car", Null)]),
                Json (Object [("name", Str "Jane"), ("age", NumD 27.3), ("car", Boolean True)]),
                Json (Object [("name", Str "Steve"), ("age", NumI 32), ("car", Boolean False)])
              ]))]
    , testCase "Complex Parse/UnParse/Parse: Array of Objects with Newlines and Tabs" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{\"clients\": [\n\t{\"name\":\"John\", \"age\":30, \"car\": null},\n\t {\"name\":\"Jane\", \"age\":27.3, \"car\": true},\n\t {\"name\":\"Steve\", \"age\":32, \"car\": false}]}"
            @?= Object [("clients", Json (Array [
                Json (Object [("name", Str "John"), ("age", NumI 30), ("car", Null)]),
                Json (Object [("name", Str "Jane"), ("age", NumD 27.3), ("car", Boolean True)]),
                Json (Object [("name", Str "Steve"), ("age", NumI 32), ("car", Boolean False)])
              ]))]
    , testCase "Complex Parse/UnParse/Parse: Delimiters in Keys and Values" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{\"ca:rs\": [\"Fo,rd\", \"B:MW\", \"F,i:at\"]}"
            @?= Object [("ca:rs",  Json (Array [Str "Fo,rd", Str "B:MW", Str "F,i:at"]))]
    , testCase "Complex Parse/UnParse/Parse: Extra Whitespace between delimiters" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{    \"cars\"    :     [\"Ford\"  ,   \"BMW\"  ,    \"Fiat\"    ]     }"
            @?= Object [("cars", Json (Array [Str "Ford", Str "BMW", Str "Fiat"]))]
    , testCase "Complex Parse/UnParse/Parse: Extra Whitespace and newlines between delimiters" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{    \"cars\"    :\n\r\t     [\"Ford\"\n\r  ,   \"BMW\"\n\r  ,    \"Fiat\"\n\r    ]\n\r\t     }"
            @?= Object [("cars", Json (Array [Str "Ford", Str "BMW", Str "Fiat"]))]
    , testCase "Complex Parse/UnParse/Parse: Whitespace between keys and object inside Array" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "[{ \"op\": \"replace\", \"path\": \"/baz\", \"value\": \"boo\" }]"
            @?= Array [Json (Object [("op",Str "replace"),("path",Str "/baz"),("value",Str "boo")])]
    , testCase "Complex Parse/UnParse/Parse: Extra Whitespace between keys and object inside Array" $ do
        (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "[ { \"op\": \"replace\", \"path\": \"/baz\", \"value\": \"boo\" }, { \"op\": \"add\", \"path\": \"/hello\", \"value\": [\"world\"] }, { \"op\": \"remove\", \"path\": \"/foo\" } ]"
            @?= Array [
                Json (Object [("op",Str "replace"),("path",Str "/baz"),("value",Str "boo")]),
                Json (Object [("op",Str "add"),("path",Str "/hello"),("value", Json (Array [Str "world"]))]),
                Json (Object [("op",Str "remove"),("path",Str "/foo")])
              ]
    , testCase "empty String as key in Object" $ do
      (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "{\"\":\"\"}"
      @?= Object [("", Str "")]
    , testCase "Empty Object" $ do
      (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "[{\n  \"foo\": \"bar\",\n  \"child\": {\n    \"grandchild\": {\n    }\n  }\n}]"
      @?= Array [Json (Object [("foo", Str "bar"), ("child", Json (Object [("grandchild", Json (Object []))]))])]
    ,testCase "Empty Array" $ do
      (parseJsonFromRoot . pack . unParseNoBeautify . parseJsonFromRoot) "[{\n  \"foo\": \"bar\",\n  \"child\": {\n    \"grandchild\": [\n    ]\n  }\n}]"
      @?= Array [Json (Object [("foo", Str "bar"), ("child", Json (Object [("grandchild", Json (Array []))]))])]


  ]