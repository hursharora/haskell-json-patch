{-# LANGUAGE OverloadedStrings #-}
module TextJsonParserSpec (
  textJsonParserTests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import TextJsonParser (parseJsonFromRoot, Json(Object, Array), Value(Str, NumI, Null, Boolean, NumD, Json))


textJsonParserTests :: TestTree
textJsonParserTests = testGroup "TextJsonParser Tests"
    [ testCase "Simple Parse: Single String" $ do
        parseJsonFromRoot "{\"name\": \"John\"}" @?= Object [("name", Str "John")]
    , testCase "Simple Parse: Single Int" $ do
        parseJsonFromRoot "{\"int\": 17}" @?= Object [("int", NumI 17)]
    , testCase "Simple Parse: Single Double" $ do
        parseJsonFromRoot "{\"double\": 17.3}" @?= Object [("double", NumD 17.3)]
    , testCase "Simple Parse: Single Bool" $ do
        parseJsonFromRoot "{\"Bool\": true}" @?= Object [("Bool", Boolean True)]
    , testCase "Simple Parse: Single Null" $ do
        parseJsonFromRoot "{\"n\": null}" @?= Object [("n", Null)]
    , testCase "Simple Parse: Multiple Items" $ do
        parseJsonFromRoot "{\"name\":\"John\", \"age\":30, \"car\": null}"
            @?= Object [("name", Str "John"), ("age", NumI 30), ("car", Null)]
    , testCase "Simple Parse: Single Array" $ do
        parseJsonFromRoot "{\"cars\": [\"Ford\", \"BMW\", \"Fiat\"]}"
            @?= Object [("cars", Json (Array [Str "Ford", Str "BMW", Str "Fiat"]))]
    , testCase "Complex Parse: Multiple Levels" $ do
        parseJsonFromRoot "{\"test\": {\"name\":\"John\", \"age\":30, \"car\": null}}"
            @?= Object [("test", Json (Object [("name", Str "John"), ("age", NumI 30), ("car", Null)]))]
    , testCase "Complex Parse: Multiple Levels, Multiple Objects" $ do
        parseJsonFromRoot "{\"test\": {\"name\":\"John\", \"age\":30, \"car\": null}, \"test2\" : {\"name\":\"Jane\", \"age\":27.3, \"car\": true}}"
            @?= Object [
              ("test", Json (Object [("name", Str "John"), ("age", NumI 30), ("car", Null)])),
              ("test2", Json (Object [("name", Str "Jane"), ("age", NumD 27.3), ("car", Boolean True)]))
            ]
    , testCase "Complex Parse: Array of Arrays" $ do
        parseJsonFromRoot "{\"numbers\": [[1,2], [3,4], [5,6]]}"
            @?= Object [("numbers", Json (Array [Json (Array [NumI 1, NumI 2]), Json (Array [NumI 3, NumI 4]), Json (Array [NumI 5, NumI 6])]))]
    , testCase "Complex Parse: Array of Objects" $ do
        parseJsonFromRoot "{\"clients\": [{\"name\":\"John\", \"age\":30, \"car\": null}, {\"name\":\"Jane\", \"age\":27.3, \"car\": true}, {\"name\":\"Steve\", \"age\":32, \"car\": false}]}"
            @?= Object [("clients", Json (Array [
                Json (Object [("name", Str "John"), ("age", NumI 30), ("car", Null)]),
                Json (Object [("name", Str "Jane"), ("age", NumD 27.3), ("car", Boolean True)]),
                Json (Object [("name", Str "Steve"), ("age", NumI 32), ("car", Boolean False)])
              ]))]
    , testCase "Complex Parse: Array of Objects with Newlines" $ do
        parseJsonFromRoot "{\"clients\": [\n{\"name\":\"John\", \"age\":30, \"car\": null},\n {\"name\":\"Jane\", \"age\":27.3, \"car\": true},\n {\"name\":\"Steve\", \"age\":32, \"car\": false}]}"
            @?= Object [("clients", Json (Array [
                Json (Object [("name", Str "John"), ("age", NumI 30), ("car", Null)]),
                Json (Object [("name", Str "Jane"), ("age", NumD 27.3), ("car", Boolean True)]),
                Json (Object [("name", Str "Steve"), ("age", NumI 32), ("car", Boolean False)])
              ]))]
    , testCase "Complex Parse: Array of Objects with Newlines and Tabs" $ do
        parseJsonFromRoot "{\"clients\": [\n\t{\"name\":\"John\", \"age\":30, \"car\": null},\n\t {\"name\":\"Jane\", \"age\":27.3, \"car\": true},\n\t {\"name\":\"Steve\", \"age\":32, \"car\": false}]}"
            @?= Object [("clients", Json (Array [
                Json (Object [("name", Str "John"), ("age", NumI 30), ("car", Null)]),
                Json (Object [("name", Str "Jane"), ("age", NumD 27.3), ("car", Boolean True)]),
                Json (Object [("name", Str "Steve"), ("age", NumI 32), ("car", Boolean False)])
              ]))]
    , testCase "Complex Parse: Delimiters in Keys and Values" $ do
        parseJsonFromRoot "{\"ca:rs\": [\"Fo,rd\", \"B:MW\", \"F,i:at\"]}"
            @?= Object [("ca:rs",  Json (Array [Str "Fo,rd", Str "B:MW", Str "F,i:at"]))]
    , testCase "Complex Parse: Extra Whitespace between delimiters" $ do
        parseJsonFromRoot "{    \"cars\"    :     [\"Ford\"  ,   \"BMW\"  ,    \"Fiat\"    ]     }"
            @?= Object [("cars", Json (Array [Str "Ford", Str "BMW", Str "Fiat"]))]
    , testCase "Complex Parse: Extra Whitespace and newlines between delimiters" $ do
        parseJsonFromRoot "{    \"cars\"    :\n\r\t     [\"Ford\"\n\r  ,   \"BMW\"\n\r  ,    \"Fiat\"\n\r    ]\n\r\t     }"
            @?= Object [("cars", Json (Array [Str "Ford", Str "BMW", Str "Fiat"]))]
    , testCase "Complex Parse: Whitespace between keys and object inside Array" $ do
        parseJsonFromRoot "[{ \"op\": \"replace\", \"path\": \"/baz\", \"value\": \"boo\" }]"
            @?= Array [Json (Object [("op",Str "replace"),("path",Str "/baz"),("value",Str "boo")])]
    , testCase "Complex Parse: Extra Whitespace between keys and object inside Array" $ do
        parseJsonFromRoot "[ { \"op\": \"replace\", \"path\": \"/baz\", \"value\": \"boo\" }, { \"op\": \"add\", \"path\": \"/hello\", \"value\": [\"world\"] }, { \"op\": \"remove\", \"path\": \"/foo\" } ]"
            @?= Array [
                Json (Object [("op",Str "replace"),("path",Str "/baz"),("value",Str "boo")]),
                Json (Object [("op",Str "add"),("path",Str "/hello"),("value", Json (Array [Str "world"]))]),
                Json (Object [("op",Str "remove"),("path",Str "/foo")])
              ]
    , testCase "empty String as key in Object" $ do
      parseJsonFromRoot "{\"\":\"\"}"
      @?= Object [("", Str "")]
    , testCase "Whitespace between end of Object in Array" $ do
      parseJsonFromRoot "[{\n  \"foo\": \"bar\",\n  \"child\": {\n    \"grandchild\": {\n    }\n  }\n}]"
      @?= Array [Json (Object [("foo", Str "bar"), ("child", Json (Object [("grandchild", Json (Object []))]))])]

  ]



