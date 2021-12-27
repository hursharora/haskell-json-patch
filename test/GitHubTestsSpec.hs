{-
*    Repository Title: JSON Patch Tests
*    File: tests.json
*    Author: Mike McCabe et al.
*    Date: May 30th 2014
*    Availability: https://github.com/json-patch/json-patch-tests/blob/master/tests.json
*    License: Copyright 2014 The Authors, Licensed under the Apache License, Version 2.0
*
*    Ported from JSON to Haskell unit tests.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module GitHubTestsSpec (
  githubTests
)
where
import System.IO
import Data.Text(Text, pack, unpack)
import Test.Tasty
import Test.Tasty.HUnit
import TextJsonParser (parseJsonFromRoot, Json(Object, Array), Value(Str, NumI, Null, Boolean, NumD, Json))
import JsonPatch(parsePatch, traverseApply, Operation(Add, Remove, Replace, Test, Copy, Move), applyPatch, parsePatch)
import Control.Exception(try, evaluate, SomeException, catch)
import Control.DeepSeq(force)
type KVPair = (Text, Value)

parseDoc = parseJsonFromRoot
parseExpected = parseJsonFromRoot

githubTests :: TestTree
githubTests = testGroup "githubTests" [
  tests
  ]

tests :: TestTree
tests = testGroup "tests.json" [
   testCase  "empty list, empty docs" $ do
    applyPatch (parsePatch "[]") (parseDoc "{}")
    @?= parseExpected "{}"
  , testCase  "empty patch list" $ do
    applyPatch (parsePatch "[]") (parseDoc "{\"foo\": 1}")
    @?= parseExpected "{\"foo\": 1}"
  , testCase "rearrangements OK?" $ do
    applyPatch (parsePatch "[]") (parseDoc "{\"foo\": 1, \"bar\": 2}")
    @?= parseExpected "{\"bar\":2, \"foo\": 1}"
  , testCase "rearrangements OK?  How about one level down ... array" $ do
    applyPatch (parsePatch "[]") (parseDoc "[{\"foo\": 1, \"bar\": 2}]")
    @?= parseExpected "[{\"bar\":2, \"foo\": 1}]"
  , testCase "rearrangements OK?  How about one level down..." $ do
    applyPatch (parsePatch "[]") (parseDoc "{\"foo\":{\"foo\": 1, \"bar\": 2}}")
    @?= parseExpected "{\"foo\":{\"bar\":2, \"foo\": 1}}"
  , testCase "add replaces any existing field" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/foo\", \"value\":1}]") (parseDoc "{\"foo\": null}")
    @?= parseExpected "{\"foo\": 1}"
  , testCase "toplevel array" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/0\", \"value\": \"foo\"}]") (parseDoc "[]")
    @?= parseExpected "[\"foo\"]"
  , testCase "toplevel array, no change" $ do
    applyPatch (parsePatch "[]") (parseDoc "[\"foo\"]")
    @?= parseExpected "[\"foo\"]"
  , testCase "toplevel object, numeric string" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/foo\", \"value\": \"1\"}]") (parseDoc "{}")
    @?= parseExpected "{\"foo\":\"1\"}"
  , testCase "toplevel object, integer" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/foo\", \"value\": 1}]") (parseDoc "{}")
    @?= parseExpected "{\"foo\":1}"
  , testCase "toplevel object, integer" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/foo\", \"value\": 1}]") (parseDoc "{}")
    @?= parseExpected "{\"foo\":1}"
  , testCase "replace object document with array document?" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"\", \"value\": []}]") (parseDoc "{}")
    @?= parseExpected "[]"
  , testCase "replace array document with object document?" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"\", \"value\": {}}]") (parseDoc "[]")
    @?= parseExpected "{}"
  , testCase "append to root array document?" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/-\", \"value\": \"hi\"}]") (parseDoc "[]")
    @?= parseExpected "[\"hi\"]"
  , testCase "Add, / target" $ do
    applyPatch (parsePatch "[ {\"op\": \"add\", \"path\": \"/\", \"value\":1 } ]") (parseDoc "{}")
    @?= parseExpected "{\"\":1}"
  , testCase "Add, /foo/ deep target (trailing slash)" $ do
    applyPatch (parsePatch "[ {\"op\": \"add\", \"path\": \"/foo/\", \"value\":1 } ]") (parseDoc "{\"foo\": {}}")
    @?= parseExpected "{\"foo\":{\"\": 1}}"
  , testCase "Add composite value at top level" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/bar\", \"value\": [1, 2]}]") (parseDoc "{\"foo\": 1}")
    @?= parseExpected "{\"foo\": 1, \"bar\": [1, 2]}"
  , testCase "Add into composite value" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/baz/0/foo\", \"value\": \"world\"}]") (parseDoc "{\"foo\": 1, \"baz\": [{\"qux\": \"hello\"}]}")
    @?= parseExpected "{\"foo\": 1, \"baz\": [{\"qux\": \"hello\", \"foo\": \"world\"}]}"
  , testCase "Out of bounds (upper)" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/bar/8\", \"value\": \"5\"}]") (parseDoc "{\"bar\": [1, 2]}")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "Out of bounds (lower)" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/bar/-1\", \"value\": \"5\"}]") (parseDoc "{\"bar\": [1, 2]}")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "Add new key and boolean true to Object" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/bar\", \"value\": true}]") (parseDoc "{\"foo\": 1}")
    @?= parseExpected "{\"foo\": 1, \"bar\": true}"
  , testCase "Add new key and boolean false to Object" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/bar\", \"value\": false}]") (parseDoc "{\"foo\": 1}")
    @?= parseExpected "{\"foo\": 1, \"bar\": false}"
  , testCase "Add new key and null value to Object" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/bar\", \"value\": null}]") (parseDoc "{\"foo\": 1}")
    @?= parseExpected "{\"foo\": 1, \"bar\": null}"
  , testCase "0 can be an array index or object element name" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/0\", \"value\": \"bar\"}]") (parseDoc "{\"foo\": 1}")
    @?= parseExpected "{\"foo\": 1, \"0\": \"bar\"}"
  , testCase "Add element to Array" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/1\", \"value\": \"bar\"}]") (parseDoc "[\"foo\"]")
    @?= parseExpected "[\"foo\", \"bar\"]"
  , testCase "Add element to Array at index 1" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/1\", \"value\": \"bar\"}]") (parseDoc "[\"foo\", \"sil\"]")
    @?= parseExpected "[\"foo\", \"bar\", \"sil\"]"
  , testCase "Add element to Array at index 0" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/0\", \"value\": \"bar\"}]") (parseDoc "[\"foo\", \"sil\"]")
    @?= parseExpected "[\"bar\", \"foo\", \"sil\"]"
  , testCase "push item to array via last index + 1" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/2\", \"value\": \"bar\"}]") (parseDoc "[\"foo\", \"sil\"]")
    @?= parseExpected "[\"foo\", \"sil\", \"bar\"]"
  , testCase "add item to array at index > length should fail"$ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[{\"op\":\"add\", \"path\": \"/3\", \"value\": \"bar\"}]") (parseDoc "[\"foo\", \"sil\"]")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "test against implementation-specific numeric parsing" $ do
    applyPatch (parsePatch "[{\"op\": \"test\", \"path\": \"/1e0\", \"value\": \"foo\"}]") (parseDoc "{\"1e0\": \"foo\"}")
    @?= parseExpected "{\"1e0\": \"foo\"}"
  , testCase "test with bad number should fail" $ do
        result <- try @SomeException (evaluate (applyPatch (parsePatch "[{\"op\": \"test\", \"path\": \"/1e0\", \"value\": \"bar\"}]") (parseDoc "[\"foo\", \"bar\"]")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "Object operation on array target" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/bar\", \"value\": 42}]") (parseDoc "[\"foo\", \"sil\"]")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "value in array add not flattened" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/1\", \"value\": [\"bar\", \"baz\"]}]") (parseDoc "[\"foo\", \"sil\"]")
    @?= parseExpected "[\"foo\", [\"bar\", \"baz\"], \"sil\"]"
  , testCase "remove" $ do
    applyPatch (parsePatch "[{\"op\": \"remove\", \"path\": \"/bar\"}]") (parseDoc "{\"foo\": 1, \"bar\": [1, 2, 3, 4]}")
    @?= parseExpected "{\"foo\": 1}"
  , testCase "remove nested" $ do
    applyPatch (parsePatch "[{\"op\": \"remove\", \"path\": \"/baz/0/qux\"}]") (parseDoc "{\"foo\": 1, \"baz\": [{\"qux\": \"hello\"}]}")
    @?= parseExpected "{\"foo\": 1, \"baz\": [{}]}"
  , testCase "replace" $ do
    applyPatch (parsePatch "[{\"op\": \"replace\", \"path\": \"/foo\", \"value\": [1, 2, 3, 4]}]") (parseDoc "{\"foo\": 1, \"baz\": [{\"qux\": \"hello\"}]}")
    @?= parseExpected "{\"foo\": [1, 2, 3, 4], \"baz\": [{\"qux\": \"hello\"}]}"
  , testCase "replace nested" $ do
    applyPatch (parsePatch "[{\"op\": \"replace\", \"path\": \"/baz/0/qux\", \"value\": \"world\"}]") (parseDoc "{\"foo\": [1, 2, 3, 4], \"baz\": [{\"qux\": \"hello\"}]}")
    @?= parseExpected "{\"foo\": [1, 2, 3, 4], \"baz\": [{\"qux\": \"world\"}]}"
  , testCase "replace array" $ do
    applyPatch (parsePatch "[{\"op\": \"replace\", \"path\": \"/0\", \"value\": \"bar\"}]") (parseDoc "[\"foo\"]")
    @?= parseExpected "[\"bar\"]"
  , testCase "replace array" $ do
    applyPatch (parsePatch "[{\"op\": \"replace\", \"path\": \"/0\", \"value\": 0}]") (parseDoc "[\"\"]")
    @?= parseExpected "[0]"
  , testCase "replace array" $ do
    applyPatch (parsePatch "[{\"op\": \"replace\", \"path\": \"/0\", \"value\": true}]") (parseDoc "[\"\"]")
    @?= parseExpected "[true]"
  , testCase "replace array" $ do
    applyPatch (parsePatch "[{\"op\": \"replace\", \"path\": \"/0\", \"value\": false}]") (parseDoc "[\"\"]")
    @?= parseExpected "[false]"
  , testCase "replace array" $ do
    applyPatch (parsePatch "[{\"op\": \"replace\", \"path\": \"/0\", \"value\": null}]") (parseDoc "[\"\"]")
    @?= parseExpected "[null]"
  , testCase "value in array replace not flattened" $ do
    applyPatch (parsePatch "[{\"op\": \"replace\", \"path\": \"/1\", \"value\": [\"bar\", \"baz\"]}]") (parseDoc "[\"foo\", \"sil\"]")
    @?= parseExpected "[\"foo\", [\"bar\", \"baz\"]]"
  , testCase "replace whole document" $ do
    applyPatch (parsePatch "[{\"op\": \"replace\", \"path\": \"\", \"value\": {\"baz\": \"qux\"}}]") (parseDoc "{\"foo\": \"bar\"}")
    @?= parseExpected "{\"baz\": \"qux\"}"
  , testCase "test replace with missing parent key should fail" $ do
        result <- try @SomeException (evaluate (applyPatch (parsePatch "[{\"op\": \"replace\", \"path\": \"/foo/bar\", \"value\": false}]") (parseDoc "{\"bar\": \"baz\"}")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "spurious patch properties" $ do
    applyPatch (parsePatch "[{\"op\": \"test\", \"path\": \"/foo\", \"value\": 1, \"spurious\": 1}]") (parseDoc "{\"foo\": 1}")
    @?= parseExpected "{\"foo\": 1}"
  , testCase "null value should be valid obj property" $ do
    applyPatch (parsePatch "[{\"op\": \"test\", \"path\": \"/foo\", \"value\": null}]") (parseDoc "{\"foo\": null}")
    @?= parseExpected "{\"foo\": null}"
  , testCase "null value should be valid obj property to be replaced with something truthy" $ do
    applyPatch (parsePatch "[{\"op\": \"replace\", \"path\": \"/foo\", \"value\": \"truthy\"}]") (parseDoc "{\"foo\": null}")
    @?= parseExpected "{\"foo\": \"truthy\"}"
  , testCase "null value should be valid obj property to be moved" $ do
    applyPatch (parsePatch "[{\"op\": \"move\", \"from\": \"/foo\", \"path\": \"/bar\"}]") (parseDoc "{\"foo\": null}")
    @?= parseExpected "{\"bar\": null}"
  , testCase "null value should be valid obj property to be copied" $ do
    applyPatch (parsePatch "[{\"op\": \"copy\", \"from\": \"/foo\", \"path\": \"/bar\"}]") (parseDoc "{\"foo\": null}")
    @?= parseExpected "{\"foo\": null, \"bar\": null}"
  , testCase "null value should be valid obj property to be removed" $ do
    applyPatch (parsePatch "[{\"op\": \"remove\", \"path\": \"/foo\"}]") (parseDoc "{\"foo\": null}")
    @?= parseExpected "{}"
  , testCase "null value should still be valid obj property replace other value" $ do
    applyPatch (parsePatch "[{\"op\": \"replace\", \"path\": \"/foo\", \"value\": null}]") (parseDoc "{\"foo\": \"bar\"}")
    @?= parseExpected "{\"foo\": null}"
  , testCase "test should pass despite rearrangement" $ do
    applyPatch (parsePatch "[{\"op\": \"test\", \"path\": \"/foo\", \"value\": {\"bar\": 2, \"foo\": 1}}]") (parseDoc "{\"foo\": {\"foo\": 1, \"bar\": 2}}")
    @?= parseExpected "{\"foo\": {\"foo\": 1, \"bar\": 2}}"
  , testCase "test should pass despite (nested) rearrangement" $ do
    applyPatch (parsePatch "[{\"op\": \"test\", \"path\": \"/foo\", \"value\": [{\"bar\": 2, \"foo\": 1}]}]") (parseDoc "{\"foo\": [{\"foo\": 1, \"bar\": 2}]}")
    @?= parseExpected "{\"foo\": [{\"foo\": 1, \"bar\": 2}]}"
  , testCase "test should pass - no error" $ do
    applyPatch (parsePatch "[{\"op\": \"test\", \"path\": \"/foo\", \"value\": {\"bar\": [1, 2, 5, 4]}}]") (parseDoc "{\"foo\": {\"bar\": [1, 2, 5, 4]}}")
    @?= parseExpected "{\"foo\": {\"bar\": [1, 2, 5, 4]}}"
  , testCase "test op should fail" $ do
        result <- try @SomeException (evaluate (applyPatch (parsePatch "[{\"op\": \"test\", \"path\": \"/foo\", \"value\": [1, 2]}]") (parseDoc "{\"foo\": {\"bar\": [1, 2, 5, 4]}}")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "Empty-string element" $ do
    applyPatch (parsePatch "[{\"op\": \"test\", \"path\": \"/\", \"value\": 1}]") (parseDoc "{ \"\": 1 }")
    @?= parseExpected "{ \"\": 1 }"
  , testCase "Complex keys object" $ do
    applyPatch
      (parsePatch "[{\"op\": \"test\", \"path\": \"/foo\", \"value\": [\"bar\", \"baz\"]},\n                {\"op\": \"test\", \"path\": \"/foo/0\", \"value\": \"bar\"},\n                {\"op\": \"test\", \"path\": \"/\", \"value\": 0},\n                {\"op\": \"test\", \"path\": \"/a~1b\", \"value\": 1},\n                {\"op\": \"test\", \"path\": \"/c%d\", \"value\": 2},\n                {\"op\": \"test\", \"path\": \"/e^f\", \"value\": 3},\n                {\"op\": \"test\", \"path\": \"/g|h\", \"value\": 4},\n                {\"op\": \"test\", \"path\":  \"/i\\\\j\", \"value\": 5},\n                {\"op\": \"test\", \"path\": \"/k\\\"l\", \"value\": 6},\n                {\"op\": \"test\", \"path\": \"/ \", \"value\": 7},\n                {\"op\": \"test\", \"path\": \"/m~0n\", \"value\": 8}]")
      (parseDoc "{\n            \"foo\": [\"bar\", \"baz\"],\n            \"\": 0,\n            \"a/b\": 1,\n            \"c%d\": 2,\n            \"e^f\": 3,\n            \"g|h\": 4,\n            \"i\\\\j\": 5,\n            \"k\\\"l\": 6,\n            \" \": 7,\n            \"m~n\": 8\n            }")
    @?= parseExpected "{\n            \"\": 0,\n            \" \": 7,\n            \"a/b\": 1,\n            \"c%d\": 2,\n            \"e^f\": 3,\n            \"foo\": [\n                \"bar\",\n                \"baz\"\n            ],\n            \"g|h\": 4,\n            \"i\\\\j\": 5,\n            \"k\\\"l\": 6,\n            \"m~n\": 8\n        }"
  , testCase "Move to same location has no effect" $ do
    applyPatch (parsePatch "[{\"op\": \"move\", \"from\": \"/foo\", \"path\": \"/foo\"}]") (parseDoc "{\"foo\": 1}")
    @?= parseExpected "{\"foo\": 1}"
  , testCase "Move - no comment" $ do
    applyPatch (parsePatch "[{\"op\": \"move\", \"from\": \"/foo\", \"path\": \"/bar\"}]") (parseDoc "{\"foo\": 1, \"baz\": [{\"qux\": \"hello\"}]}")
    @?= parseExpected "{\"baz\": [{\"qux\": \"hello\"}], \"bar\": 1}"
  , testCase "Move - no comment 2" $ do
    applyPatch (parsePatch "[{\"op\": \"move\", \"from\": \"/baz/0/qux\", \"path\": \"/baz/1\"}]") (parseDoc "{\"baz\": [{\"qux\": \"hello\"}], \"bar\": 1}")
    @?= parseExpected "{\"baz\": [{}, \"hello\"], \"bar\": 1}"
  , testCase "Copy" $ do
    applyPatch (parsePatch "[{\"op\": \"copy\", \"from\": \"/baz/0\", \"path\": \"/boo\"}]") (parseDoc "{\"baz\": [{\"qux\": \"hello\"}], \"bar\": 1}")
    @?= parseExpected "{\"baz\":[{\"qux\":\"hello\"}],\"bar\":1,\"boo\":{\"qux\":\"hello\"}}"
  , testCase "replacing the root of the document is possible with add" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"\", \"value\": {\"baz\": \"qux\"}}]") (parseDoc "{\"foo\": \"bar\"}")
    @?= parseExpected "{\"baz\":\"qux\"}"
  , testCase "Adding to \"/-\" adds to the end of the array" $ do
    applyPatch (parsePatch "[ { \"op\": \"add\", \"path\": \"/-\", \"value\": { \"foo\": [ \"bar\", \"baz\" ] } } ]") (parseDoc "[ 1, 2 ]")
    @?= parseExpected "[ 1, 2, { \"foo\": [ \"bar\", \"baz\" ] } ]"
  , testCase "Adding to \"/-\" adds to the end of the array, even n levels down" $ do
    applyPatch (parsePatch "[ { \"op\": \"add\", \"path\": \"/2/1/-\", \"value\": { \"foo\": [ \"bar\", \"baz\" ] } } ]") (parseDoc "[ 1, 2, [ 3, [ 4, 5 ] ] ]")
    @?= parseExpected "[ 1, 2, [ 3, [ 4, 5, { \"foo\": [ \"bar\", \"baz\" ] } ] ] ]"
  , testCase "test remove with bad number should fail" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[{\"op\": \"remove\", \"path\": \"/baz/1e0/qux\"}]") (parseDoc "{\"foo\": 1, \"baz\": [{\"qux\": \"hello\"}]}")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "test remove on array" $ do
    applyPatch (parsePatch "[{\"op\": \"remove\", \"path\": \"/0\"}]") (parseDoc "[1, 2, 3, 4]")
    @?= parseExpected "[2, 3, 4]"
  , testCase "test repeated removes" $ do
    applyPatch (parsePatch "[{ \"op\": \"remove\", \"path\": \"/1\" },\n                { \"op\": \"remove\", \"path\": \"/2\" }]") (parseDoc "[1, 2, 3, 4]")
    @?= parseExpected "[1, 3]"
  , testCase "remove op shouldn't remove from array with bad number" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[{\"op\": \"remove\", \"path\": \"/1e0\"}]") (parseDoc "[1, 2, 3, 4]")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "test replace with bad number should fail" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[{\"op\": \"replace\", \"path\": \"/1e0\", \"value\": false}]") (parseDoc "[\"\"]")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "test copy with bad number should fail" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[{\"op\": \"copy\", \"from\": \"/baz/1e0\", \"path\": \"/boo\"}]") (parseDoc "{\"baz\": [1,2,3], \"bar\": 1}")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "test move with bad number should fail" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[{\"op\": \"move\", \"from\": \"/baz/1e0\", \"path\": \"/foo\"}]") (parseDoc "{\"foo\": 1, \"baz\": [1,2,3,4]}")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "test add with bad number should fail" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/1e0\", \"value\": \"bar\"}]") (parseDoc "[\"foo\", \"sil\"]")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "missing 'path' parameter" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[ { \"op\": \"add\", \"value\": \"bar\" } ]") (parseDoc "{}")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "'path' parameter with null value" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[ { \"op\": \"add\", \"path\": null, \"value\": \"bar\" } ]") (parseDoc "{}")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "invalid JSON Pointer token" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[ { \"op\": \"add\", \"path\": \"foo\", \"value\": \"bar\" } ]") (parseDoc "{}")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "missing 'value' parameter to add" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[ { \"op\": \"add\", \"path\": \"/-\" } ]") (parseDoc "[ 1 ]")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "missing 'value' parameter to replace" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[ { \"op\": \"replace\", \"path\": \"/0\" } ]") (parseDoc "[ 1 ]")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "missing 'value' parameter to test" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[ { \"op\": \"test\", \"path\": \"/0\" } ]") (parseDoc "[ null ]")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "missing value parameter to test - where undef is falsy" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[ { \"op\": \"test\", \"path\": \"/0\" } ]") (parseDoc "[ false ]")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "missing from parameter to copy" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[ { \"op\": \"copy\", \"path\": \"/-\" } ]") (parseDoc "[ 1 ]")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "missing from parameter to copy" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[ { \"op\": \"copy\", \"from\": \"/bar\", \"path\": \"/foo\" } ]") (parseDoc "{ \"foo\": 1 }")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "missing from parameter to move" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[ { \"op\": \"move\", \"path\": \"\" } ]") (parseDoc "{ \"foo\": 1 }")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "missing from location to move" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[ { \"op\": \"move\", \"from\": \"/bar\", \"path\": \"/foo\" } ]") (parseDoc "{ \"foo\": 1 }")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "unrecognized op should fail" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[{\"op\": \"spam\", \"path\": \"/foo\", \"value\": 1}]") (parseDoc "{ \"foo\": 1 }")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "test with bad array number that has leading zeros" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[{\"op\": \"test\", \"path\": \"/00\", \"value\": \"foo\"}]") (parseDoc "[\"foo\", \"bar\"]")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "Removing nonexistent field" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[{\"op\": \"remove\", \"path\": \"/baz\"}]") (parseDoc "{\"foo\" : \"bar\"}")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "Removing deep nonexistent path" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[{\"op\": \"remove\", \"path\": \"/missing1/missing2\"}]") (parseDoc "{\"foo\" : \"bar\"}")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "Removing nonexistent index" $ do
        result <- try @SomeException (evaluate $ force (applyPatch (parsePatch "[{\"op\": \"remove\", \"path\": \"/2\"}]") (parseDoc "[\"foo\", \"bar\"]")))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  , testCase "Patch with different capitalisation than doc" $ do
    applyPatch (parsePatch "[{\"op\": \"add\", \"path\": \"/FOO\", \"value\": \"BAR\"}]") (parseDoc "{\"foo\":\"bar\"}")
    @?= parseExpected "{\"foo\": \"bar\", \"FOO\": \"BAR\"}"
  ]
