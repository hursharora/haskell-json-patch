import Lib
import Test.Tasty
import Test.Tasty.HUnit
import TextJsonParserSpec(textJsonParserTests)
import JsonPatchSpec(jsonPatchTests)
import GitHubTestsSpec(githubTests)
import BeautifierSpec(beautifierTests)

main :: IO ()
main = defaultMain $ testGroup "Tests" [
    textJsonParserTests,
    jsonPatchTests,
    githubTests,
    beautifierTests
  ]
