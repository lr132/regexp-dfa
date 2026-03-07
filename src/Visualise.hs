module Visualise (toDot, toAsciiTable) where

import Types
import qualified Data.Map.Strict as M
import Data.List (sort)

-- Generate a Graphviz DOT string for a DFA
toDot :: DFA -> String
toDot dfa = unlines $
  [ "digraph DFA {"
  , "  rankdir=LR;"
  , "  \"\" [shape=none label=\"\"];"
  , "  \"\" -> " ++ show (dfaStart dfa) ++ ";"
  ] ++
  [ "  " ++ show s ++ " [shape=" ++ shape s ++ " label=\"" ++ show s ++ "\"];"
  | s <- sort (dfaStates dfa) ] ++
  [ "  " ++ show from ++ " -> " ++ show to ++ " [label=\"" ++ [c] ++ "\"];"
  | ((from, c), to) <- M.toAscList (dfaTrans dfa) ] ++
  [ "}" ]
  where
    shape s
      | s `elem` dfaAccept dfa = "doublecircle"
      | otherwise               = "circle"

-- Generate an ASCII transition table
toAsciiTable :: DFA -> String
toAsciiTable dfa = unlines $
  [ header
  , separator
  ] ++ rows
  where
    alpha   = dfaAlpha dfa
    states  = sort (dfaStates dfa)
    start   = dfaStart dfa
    accepts = dfaAccept dfa

    colW   = 6
    stateW = 8

    pad n s = take n (s ++ repeat ' ')

    stateLabel s =
      let arrow = if s == start        then "-> " else "   "
          star  = if s `elem` accepts  then "*"   else " "
      in arrow ++ show s ++ star

    header    = pad stateW "State" ++ concatMap (\c -> pad colW ['|', ' ', c]) alpha
    separator = replicate stateW '-' ++ concat (replicate (length alpha) (replicate colW '-'))

    rows = [ pad stateW (stateLabel s) ++
             concatMap (\c -> pad colW ("|" ++ cell s c)) alpha
           | s <- states ]

    cell s c = case M.lookup (s, c) (dfaTrans dfa) of
      Just t  -> " " ++ show t
      Nothing -> " -"
