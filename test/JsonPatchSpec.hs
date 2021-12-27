{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module JsonPatchSpec (
  jsonPatchTests
) where
import Test.Tasty
import Test.Tasty.HUnit
import TextJsonParser (parseJsonFromRoot, Json(Object, Array), Value(Str, NumI, Null, Boolean, NumD, Json))
import JsonPatch(parsePatch, traverseApply, Operation(Add, Remove, Replace, Test, Copy, Move))
import Control.Exception(try, evaluate, SomeException)

jsonPatchTests :: TestTree
jsonPatchTests = testGroup "JsonPatch Tests" [
    jsonPatchParsingTests,
    jsonPatchAddTests,
    jsonPatchTestTests,
    jsonPatchRemoveTests,
    jsonPatchReplaceTests,
    jsonPatchCopyTests,
    jsonPatchMoveTests,
    jsonPatchRearrangementTests,
    jsonPatchEscapedCharTests
  ]

jsonPatchParsingTests :: TestTree
jsonPatchParsingTests = testGroup "jsonPatch: Parsing Tests"
    [ testCase "Parse Patch" $ do
       parsePatch "[{ \"op\": \"replace\", \"path\": \"/baz\", \"value\": \"boo\" }, { \"op\": \"add\", \"path\": \"/hello\", \"value\": [\"world\"] },{ \"op\": \"remove\", \"path\": \"/foo\" }]"
       @?= [
        Replace "/baz" (Str "boo"),
        Add "/hello" (Json (Array [Str "world"])),
        Remove "/foo"
      ]
    ]

jsonPatchAddTests :: TestTree
jsonPatchAddTests = testGroup "jsonPatch: Add Operation Tests" [
  testCase "Operation Add: add to empty object " $ do
        traverseApply (Add "/baz" (Str "foo")) (Object [])
        @?= Object [("baz", Str "foo")]
      , testCase "Operation Add: add to non-empty object" $ do
        traverseApply (Add "/qux" (Str "bar")) (Object [("baz", Str "foo")])
        @?= Object [("baz", Str "foo"), ("qux", Str "bar") ]
      , testCase "Operation Add: add to empty array" $ do
        traverseApply (Add "/0" (Str "foo")) (Array [])
        @?= Array [Str "foo"]
      , testCase "Operation Add: add to non-empty array" $ do
        traverseApply (Add "/0" (Str "foo")) (Array [Str "baz"])
        @?= Array [Str "foo", Str "baz"]
      , testCase "Operation Add: nested add array at given index" $ do
        traverseApply (Add "/1/bar" (Str "foo")) (Array [Str "0", Json (Object []), Str "2"])
        @?= Array [Str "0", Json (Object [("bar", Str "foo")]) , Str "2"]
      , testCase "Operation Add: non-existant objects are not added recursively" $ do
        result <- try @SomeException (evaluate (traverseApply (Add "/2/bar" (Str "foo")) (Array [Str "0", Str "1"])))
        case result of
          Left ex -> return ()
          Right v -> error "failed"
  ]

jsonPatchTestTests :: TestTree
jsonPatchTestTests = testGroup "jsonPatch: Test Operation Tests" [
  testCase "Operation Test: simple test on object" $ do
        traverseApply (Test "/baz" (Str "foo")) (Object [("baz", Str "foo")])
        @?= Object [("baz", Str "foo")]
      , testCase "Operation Test: complex test on object" $ do
        traverseApply (Test "/baz/bar" (Str "foo")) (Object [("baz", Json (Object [("bar", Str "foo")]))])
        @?= Object [("baz", Json (Object [("bar", Str "foo")]))]
      , testCase "Operation Test: order indifference test on object" $ do
        traverseApply (Test "/baz/bar" (Str "foo")) (Object [("qux", Json (Object [("quux", Str "foo")])), ("baz", Json (Object [("bar", Str "foo")]))])
        @?= Object [("qux", Json (Object [("quux", Str "foo")])), ("baz", Json (Object [("bar", Str "foo")]))]
      , testCase "Operation Test: simple test on array" $ do
        traverseApply (Test "/0" (Str "foo")) (Array [Str "foo"])
        @?= Array [Str "foo"]
      , testCase "Operation Test: end of array \"-\" test on array" $ do
        traverseApply (Test "/-" (Str "foo")) (Array [Str "foo"])
        @?= Array [Str "foo"]
      , testCase "Operation Test: complex end of array \"-\" test on array" $ do
        traverseApply (Test "/-" (Str "foo")) (Array [Str "bar", Str "baz" ,Str "foo"])
        @?= Array [Str "bar", Str "baz" ,Str "foo"]
      , testCase "Operation Test: end of array \"-\" test on array" $ do
        traverseApply (Test "/baz/-" (Str "foo")) (Object [("baz", Json (Array [Str "bar", Str "baz" ,Str "foo"]))])
        @?= Object [("baz", Json (Array [Str "bar", Str "baz" ,Str "foo"]))]
  ]

jsonPatchRemoveTests :: TestTree
jsonPatchRemoveTests = testGroup "jsonPatch: Remove Operation Tests" [
  testCase "Remove from object" $ do
    traverseApply (Remove "/baz") (Object [("baz", Str "foo")])
    @?= Object []
  , testCase "Remove from array" $ do
    traverseApply (Remove "/0") (Array [Str "foo"])
    @?= Array []
  , testCase "Remove from object - nested" $ do
    traverseApply (Remove "/baz/foo") (Object [("baz", Json (Object [("foo", Str "hello")]))])
    @?= Object [("baz", Json (Object []))]
  , testCase "Remove from array - nested" $ do
    traverseApply (Remove "/0/foo") (Array [Json (Object [("foo", Str "hello")])])
    @?= Array [Json (Object [])]
  , testCase "Remove from object - complex" $ do
    traverseApply (Remove "/bar/qux/1/foo") (Object [("bar", Json (Object [("abc", NumI 2), ("qux", Json (Array [Str "xyz", Json (Object [("foo", NumD 3.2)])]))])), ("quux", Null)])
    @?= Object [("bar", Json (Object [("abc", NumI 2), ("qux", Json (Array [Str "xyz", Json (Object [])]))])), ("quux", Null)]
  ]

jsonPatchReplaceTests :: TestTree
jsonPatchReplaceTests = testGroup "jsonPatch: Replace Operation Tests" [
   testCase "Replace object field" $ do
    traverseApply (Replace "/baz" Null) (Object [("baz", Str "foo")])
    @?= Object [("baz", Null)]
  , testCase "Replace array member" $ do
    traverseApply (Replace "/0" Null) (Array [Str "foo"])
    @?= Array [Null]
  , testCase "Replace object field - nested" $ do
    traverseApply (Replace "/baz/bar" (NumD 2.4)) (Object [("baz", Json (Object [("bar", Str "foo")]))])
    @?= Object [("baz", Json (Object [("bar", NumD 2.4)]))]
  , testCase "Replace array member - nested" $ do
    traverseApply (Replace "/0/bar" (NumD 2.4)) (Array [Json (Object [("bar", Str "foo")])])
    @?= Array [Json (Object [("bar", NumD 2.4)])]
  ]

jsonPatchCopyTests :: TestTree
jsonPatchCopyTests = testGroup "jsonPatch: Copy Operation Tests" [
    testCase "Copy object one field to other" $ do
     traverseApply (Copy "/src" "/dest") (Object [("src", Str "value"), ("dest", Null)])
     @?= Object [("src", Str "value"), ("dest", Str "value")]
  , testCase "Copy array element to other" $ do
     traverseApply (Copy "/0" "/1") (Array [Str "value", Null])
     @?= Array [Str "value", Str "value", Null]
  ]

jsonPatchMoveTests :: TestTree
jsonPatchMoveTests = testGroup "jsonPatch: Move Operation Tests" [
    testCase "Move object one field to other" $ do
     traverseApply (Move "/biscuits/0" "/best_biscuit") (Object [("biscuits", Json (Array [Str "Chocolate Digestive"]))])
     @?= Object [("biscuits", Json (Array [])), ("best_biscuit", Str "Chocolate Digestive")]
  ]

jsonPatchRearrangementTests :: TestTree
jsonPatchRearrangementTests = testGroup "re-arrangement Tests" [
    testCase "Object: re-arrange fields" $ do
      Object [("abc", Str "baz"), ("biscuits", Json (Array [Str "Chocolate Digestive"]))] == Object [("biscuits", Json (Array [Str "Chocolate Digestive"])), ("abc", Str "baz")]
     @?= True
    , testCase "Array: re-arrange elements" $ do
      Array [Str "baz", Json (Array [Str "Chocolate Digestive"])] == Array [Json (Array [Str "Chocolate Digestive"]), Str "baz"]
     @?= False
    , testCase "Json: equality for nested array" $ do
      Json (Array [Null, Str "Chocolate Digestive"]) == Json (Array [Null, Str "Chocolate Digestive"])
     @?= True
  ]

jsonPatchEscapedCharTests :: TestTree
jsonPatchEscapedCharTests = testGroup "escaped characters Tests" [
     testCase "Object ~" $ do
        traverseApply (Test "/baz~0bar" (Str "foo")) (Object [("baz~bar", Str "foo")])
        @?= Object [("baz~bar", Str "foo")]
    , testCase "Object /" $ do
        traverseApply (Test "/baz~1bar" (Str "foo")) (Object [("baz/bar", Str "foo")])
        @?= Object [("baz/bar", Str "foo")]
    , testCase "Object / ~" $ do
        traverseApply (Test "/baz~0b~1ar" (Str "foo")) (Object [("baz~b/ar", Str "foo")])
        @?= Object [("baz~b/ar", Str "foo")]
  ]
