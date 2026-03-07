module DFA (toDFA) where

import Types
import qualified Data.Map.Strict as M
import qualified Data.Set as Set

type SS = Set.Set Int

toDFA :: NFA -> DFA
toDFA nfa = DFA (M.elems numbering) alpha dfaTransMap startIdx accepts
  where
    alpha   = nfaAlpha nfa
    startSS = epsClosure (Set.singleton (nfaStart nfa))

    (ssToIdx, transList) = explore M.empty [] [startSS]

    numbering :: M.Map Int Int
    numbering = M.fromList [(idx, idx) | idx <- M.elems ssToIdx]

    dfaTransMap = M.fromList
      [ ((ssToIdx M.! from, c), ssToIdx M.! to)
      | (from, c, to) <- transList ]

    accepts = [ ssToIdx M.! ss
              | ss <- M.keys ssToIdx
              , any (`Set.member` ss) (nfaAccept nfa) ]

    startIdx = ssToIdx M.! startSS

    epsClosure :: SS -> SS
    epsClosure initial = go initial initial
      where
        go seen frontier
          | Set.null frontier = seen
          | otherwise =
              let next = Set.fromList
                           [ t | s <- Set.toList frontier
                               , (from, Eps, t) <- nfaTrans nfa
                               , from == s ]
                  newStates = Set.difference next seen
              in go (Set.union seen newStates) newStates

    move :: SS -> Char -> SS
    move states c = Set.fromList
      [ t | s <- Set.toList states
          , (from, lbl, t) <- nfaTrans nfa
          , from == s
          , lbl == OnChar c || lbl == OnAny ]

    explore :: M.Map SS Int -> [(SS, Char, SS)] -> [SS] -> (M.Map SS Int, [(SS, Char, SS)])
    explore ssMap trans [] = (ssMap, trans)
    explore ssMap trans (ss:queue)
      | ss `M.member` ssMap = explore ssMap trans queue
      | otherwise =
          let idx    = M.size ssMap
              ssMap' = M.insert ss idx ssMap
              nexts  = [ (c, epsClosure (move ss c)) | c <- alpha ]
              newT   = [ (ss, c, next) | (c, next) <- nexts ]
              newQ   = [ next | (_, next) <- nexts, next `M.notMember` ssMap' ]
          in explore ssMap' (trans ++ newT) (queue ++ newQ)
