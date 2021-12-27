{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Beautifier (unParse, unParseNoBeautify)
import Control.Monad ( unless, when )
import Data.Data (Data (toConstr), Typeable)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (pack)
import JsonPatch (applyPatch, parsePatch)
import System.Console.GetOpt
    ( getOpt,
      usageInfo,
      ArgDescr(OptArg, ReqArg),
      ArgOrder(Permute),
      OptDescr(..) )
import System.Environment (getArgs)
import TextJsonParser (parseJsonFromRoot)

data Flag
  = Infile String
  | Outfile String
  | Patch String
  | Beautify String
  deriving (Show, Eq, Typeable, Data)

header :: String
header = "Usage: Main --infile FILE [OPTIONS...]"

options :: [OptDescr Flag]
options =
  [ Option ['i'] ["infile"] (ReqArg Infile "FILE") "specify input FILE",
    Option ['o'] ["outfile"] (ReqArg Outfile "FILE") "specify output FILE",
    Option ['p'] ["patch"] (ReqArg Patch "FILE") "specify patch FILE",
    Option ['b'] ["beautify"] (OptArg beut "SPACING") "beautify output, optionally specify spacing"
  ]

beut :: Maybe String -> Flag
beut = Beautify . fromMaybe "2"

getOptions :: [String] -> IO ([Flag], [String])
getOptions argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

main :: IO ()
main = do
  args <- getArgs
  (actions, nonOptions) <- getOptions args

  let valid = validate actions
  unless
    valid
    (ioError (userError ("Invalid usage\n" ++ usageInfo header options)))

  let infilePath = getActionValue (Infile "X") actions
  let patchPath = getActionValue (Patch "X") actions
  let outFilePath = if Outfile "X" `flagElem` actions
      then getActionValue (Outfile "X") actions
      else infilePath

  inFile <- readFile infilePath
  patchFile <- if Patch "X" `flagElem` actions
    then readFile patchPath
    else return "[]"

  let infileJson = parseJsonFromRoot (pack inFile)
  let patchOps = parsePatch (pack patchFile)

  let resultJson = applyPatch patchOps infileJson

  let writeContent = if Beautify "X" `flagElem` actions
      then unParse (replicate (read (getActionValue (Beautify "X") actions)) ' ') resultJson
      else unParseNoBeautify resultJson

  when (length writeContent > 0) $
    writeFile outFilePath writeContent

  putStrLn "Done!"

getActionValue :: Flag -> [Flag] -> String
getActionValue f ac = getFlagVal (head (filter (\a -> toConstr a == toConstr f) ac))

getFlagVal :: Flag -> String
getFlagVal (Infile s) = s
getFlagVal (Outfile s) = s
getFlagVal (Patch s) = s
getFlagVal (Beautify s) = s

-- Validation functions below

validate :: [Flag] -> Bool
validate actions = all (`flagElem` actions) [Infile "X"]

flagElem :: Flag -> [Flag] -> Bool
flagElem _ [] = False
flagElem f (f0 : fs) = toConstr f == toConstr f0 || flagElem f fs
