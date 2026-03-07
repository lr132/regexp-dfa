module DFFile (serialiseDF, deserialiseDF) where

import Types
import qualified Data.Map.Strict as M
import Data.List (intercalate)

-- Serialise a regex string + DFA to .df file content
serialiseDF :: String -> DFA -> String
serialiseDF regexp dfa = unlines $
  [ "# regexp-dfa v1"
  , "regexp: " ++ regexp
  , "alphabet: " ++ unwords (map (:[]) (dfaAlpha dfa))
  , "states: " ++ unwords (map show (dfaStates dfa))
  , "initial: " ++ show (dfaStart dfa)
  , "accepting: " ++ unwords (map show (dfaAccept dfa))
  , "transitions:"
  ] ++
  [ unwords [show from, [c], show to]
  | ((from, c), to) <- M.toAscList (dfaTrans dfa) ]

-- Parse a .df file back into (regexp string, DFA)
deserialiseDF :: String -> Either String (String, DFA)
deserialiseDF content = do
  let ls = filter (not . isComment) . map trim . lines $ content
  regexp   <- need "regexp"   ls
  alphaStr <- need "alphabet" ls
  statesStr <- need "states"  ls
  initStr  <- need "initial"  ls
  accStr   <- need "accepting" ls
  let alpha   = map head . words $ alphaStr
      states  = map read . words $ statesStr
      start   = read initStr
      accepts = map read . words $ accStr
      transLines = drop 1 . dropWhile (/= "transitions:") $ ls
      trans = M.fromList
        [ ((read from, head on), read to)
        | [from, on, to] <- map words transLines
        , not (null on) ]
  Right (regexp, DFA states alpha trans start accepts)
  where
    need key ls =
      case [drop (length key + 2) l | l <- ls, (key ++ ": ") `isPrefixOf` l] of
        (v:_) -> Right v
        []    -> Left ("Missing field: " ++ key)
    isComment l = case trim l of { ('#':_) -> True; "" -> True; _ -> False }
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
    isPrefixOf [] _      = True
    isPrefixOf _ []      = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
