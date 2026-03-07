module Main where

import GHC.Wasm.Prim
import Types
import Parser      (parseRegExp)
import Unparser    (unparseRegExp)
import NFA         (toNFA, extractAlpha, inlineLet)
import DFA         (toDFA)
import Visualise   (toDot, toAsciiTable)
import BoundedLang (boundedLang)
import Matcher     (matchesRegex)
import qualified Data.Map.Strict as M

main :: IO ()
main = return ()

-- Compile a regex and return a JSON object with all relevant info.
-- On error returns: {"error":"..."}
-- On success returns: {"dot":"...","table":"...","states":N,"start":N,"accepting":[...],"alphabet":"...","examples":[...],"regexp":"..."}
foreign export javascript "hs_compile"
  hsCompile :: JSString -> IO JSString

hsCompile :: JSString -> IO JSString
hsCompile jsInput = do
  let input = fromJSString jsInput
  case compile input of
    Left err -> return $ toJSString $ jsonError err
    Right (lre, dfa) -> do
      let dot     = toDot dfa
          table   = toAsciiTable dfa
          states  = length (dfaStates dfa)
          start   = dfaStart dfa
          accepts = dfaAccept dfa
          alpha   = dfaAlpha dfa
          exs     = take 12 (boundedLang 8 lre)
          regexp  = unparseRegExp lre
      return $ toJSString $ jsonObj
        [ ("regexp",    jsonStr regexp)
        , ("dot",       jsonStr dot)
        , ("table",     jsonStr table)
        , ("states",    show states)
        , ("start",     show start)
        , ("accepting", jsonArr (map show accepts))
        , ("alphabet",  jsonStr (map (\c -> c) alpha))
        , ("examples",  jsonArr (map jsonStr exs))
        ]

-- Test a string against a regex.
-- Returns: {"match":true} or {"match":false} or {"error":"..."}
foreign export javascript "hs_test"
  hsTest :: JSString -> JSString -> IO JSString

hsTest :: JSString -> JSString -> IO JSString
hsTest jsStr jsInput = do
  let str   = fromJSString jsStr
      input = fromJSString jsInput
  case parseRegExp (filter (/= ' ') input) of
    Nothing -> return $ toJSString $ jsonError "Could not parse regex"
    Just re ->
      let result = matchesRegex str re
      in return $ toJSString $ jsonObj [("match", if result then "true" else "false")]

-- Generate example matches up to a given length.
-- Returns: {"examples":["AB","C",...]} or {"error":"..."}
foreign export javascript "hs_examples"
  hsExamples :: JSString -> JSString -> IO JSString

hsExamples :: JSString -> JSString -> IO JSString
hsExamples jsN jsInput = do
  let n     = read (fromJSString jsN) :: Int
      input = fromJSString jsInput
  case parseRegExp (filter (/= ' ') input) of
    Nothing -> return $ toJSString $ jsonError "Could not parse regex"
    Just re ->
      let exs = boundedLang n re
      in return $ toJSString $ jsonObj [("examples", jsonArr (map jsonStr exs))]

-- Core pipeline (mirrors app/Main.hs compile)
compile :: String -> Either String (LRegExp, DFA)
compile input = do
  let stripped = filter (/= ' ') input
  lre <- maybe (Left "Parse error: invalid regex") Right (parseRegExp stripped)
  re  <- inlineLet lre
  let alpha = if null (extractAlpha lre) then ['A'..'Z'] else extractAlpha lre
      nfa   = toNFA alpha re
      dfa   = toDFA nfa
  Right (lre, dfa)

-- Minimal JSON helpers (no aeson dependency)
jsonStr :: String -> String
jsonStr s = "\"" ++ concatMap escape s ++ "\""
  where
    escape '"'  = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c    = [c]

jsonArr :: [String] -> String
jsonArr xs = "[" ++ intercalate "," xs ++ "]"

jsonObj :: [(String, String)] -> String
jsonObj pairs = "{" ++ intercalate "," (map pair pairs) ++ "}"
  where pair (k, v) = "\"" ++ k ++ "\":" ++ v

jsonError :: String -> String
jsonError msg = jsonObj [("error", jsonStr msg)]

intercalate :: String -> [String] -> String
intercalate _ []     = ""
intercalate _ [x]    = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs
