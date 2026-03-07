module NFA (toNFA, extractAlpha, inlineLet) where

import Types
import qualified Data.Map.Strict as M
import Data.List (nub, sort)

-- Extract literal characters used in a regex (the effective alphabet)
extractAlpha :: LRegExp -> [Char]
extractAlpha (Let bs re) = sort . nub $ goR re ++ concatMap (goR . snd) bs
  where
    goR (S sr)    = goS sr
    goR (Star sr) = goS sr
    goR (Plus sr) = goS sr
    goR (Seq rs)  = concatMap goR rs
    goR (AltR rs) = concatMap goR rs
    goR (StarR r) = goR r
    goR (PlusR r) = goR r
    goS (Lit c)     = [c]
    goS (Opt c)     = [c]
    goS (SSeq ss)   = concatMap goS ss
    goS (Choice ss) = concatMap goS ss
    goS _           = []

-- Inline let bindings. Returns Left on recursive bindings.
inlineLet :: LRegExp -> Either String RegExp
inlineLet (Let [] re) = Right re
inlineLet (Let bs re) =
  let env = M.fromList bs
  in case inlineR M.empty env re of
       Just r  -> Right r
       Nothing -> Left "Recursive let bindings cannot be converted to DFA"
  where
    inlineR seen env (S sr)    = S    <$> inlineS seen env sr
    inlineR seen env (Star sr) = Star <$> inlineS seen env sr
    inlineR seen env (Plus sr) = Plus <$> inlineS seen env sr
    inlineR seen env (Seq rs)  = Seq  <$> mapM (inlineR seen env) rs
    inlineR seen env (AltR rs) = AltR <$> mapM (inlineR seen env) rs
    inlineR seen env (StarR r) = StarR <$> inlineR seen env r
    inlineR seen env (PlusR r) = PlusR <$> inlineR seen env r

    inlineS seen env (Var n)
      | n `M.member` seen = Nothing
      | otherwise = case M.lookup n env of
          Nothing -> Just (Var n)
          Just r  -> r2s <$> inlineR (M.insert n () seen) env r
    inlineS seen env (SSeq ss)   = SSeq   <$> mapM (inlineS seen env) ss
    inlineS seen env (Choice ss) = Choice <$> mapM (inlineS seen env) ss
    inlineS _ _ sr = Just sr

    r2s (S sr)    = sr
    r2s (Seq rs)  = SSeq (map r2s rs)
    r2s (Star sr) = sr
    r2s (Plus sr) = sr
    r2s (AltR rs) = Choice (map r2s rs)
    r2s (StarR r) = r2s r
    r2s (PlusR r) = r2s r

-- Build NFA via Thompson's construction
toNFA :: [Char] -> RegExp -> NFA
toNFA alpha re =
  let ((start, accept, trans), n) = buildR 0 re
  in NFA [0..n-1] alpha trans start [accept]

type Frag = (Int, Int, [(Int, Label, Int)])

buildR :: Int -> RegExp -> (Frag, Int)
buildR n (S sr)    = buildS n sr
buildR n (Star sr) =
  let ((s, a, ts), n1) = buildS n sr
      ns = n1; na = n1 + 1
      new = [(ns, Eps, s), (ns, Eps, na), (a, Eps, s), (a, Eps, na)]
  in ((ns, na, ts ++ new), n1 + 2)
buildR n (Plus sr) =
  let ((s, a, ts), n1) = buildS n sr
      ns = n1; na = n1 + 1
      new = [(ns, Eps, s), (a, Eps, s), (a, Eps, na)]
  in ((ns, na, ts ++ new), n1 + 2)
buildR n (Seq [])     = buildS n Empty
buildR n (Seq [r])    = buildR n r
buildR n (Seq (r:rs)) =
  let ((s1, a1, ts1), n1) = buildR n r
      ((s2, a2, ts2), n2) = buildR n1 (Seq rs)
  in ((s1, a2, ts1 ++ ts2 ++ [(a1, Eps, s2)]), n2)
buildR n (StarR r) =
  let ((s, a, ts), n1) = buildR n r
      ns = n1; na = n1 + 1
      new = [(ns, Eps, s), (ns, Eps, na), (a, Eps, s), (a, Eps, na)]
  in ((ns, na, ts ++ new), n1 + 2)
buildR n (PlusR r) =
  let ((s, a, ts), n1) = buildR n r
      ns = n1; na = n1 + 1
      new = [(ns, Eps, s), (a, Eps, s), (a, Eps, na)]
  in ((ns, na, ts ++ new), n1 + 2)
buildR n (AltR [])    = buildS n Empty
buildR n (AltR [r])   = buildR n r
buildR n (AltR rs)    =
  let (frags, n1) = buildAllR n rs
      ns = n1; na = n1 + 1
      ins  = [(ns, Eps, s) | (s, _, _) <- frags]
      outs = [(a, Eps, na) | (_, a, _) <- frags]
      ts   = concatMap (\(_, _, t) -> t) frags
  in ((ns, na, ts ++ ins ++ outs), n1 + 2)

buildAllR :: Int -> [RegExp] -> ([Frag], Int)
buildAllR n []     = ([], n)
buildAllR n (r:rs) =
  let (frag, n')     = buildR n r
      (rest, nFinal) = buildAllR n' rs
  in (frag : rest, nFinal)

buildS :: Int -> SRegExp -> (Frag, Int)
buildS n Empty  = let s=n; a=n+1 in ((s, a, [(s, Eps, a)]),           n+2)
buildS n (Lit c)= let s=n; a=n+1 in ((s, a, [(s, OnChar c, a)]),      n+2)
buildS n (Opt c)= let s=n; a=n+1 in ((s, a, [(s, OnChar c, a),(s, Eps, a)]), n+2)
buildS n Any    = let s=n; a=n+1 in ((s, a, [(s, OnAny, a)]),          n+2)
buildS n OptAny = let s=n; a=n+1 in ((s, a, [(s, OnAny, a),(s, Eps, a)]), n+2)
buildS n (Var _)= buildS n Empty
buildS n (SSeq [])       = buildS n Empty
buildS n (SSeq [sr])     = buildS n sr
buildS n (SSeq (sr:srs)) =
  let ((s1, a1, ts1), n1) = buildS n sr
      ((s2, a2, ts2), n2) = buildS n1 (SSeq srs)
  in ((s1, a2, ts1 ++ ts2 ++ [(a1, Eps, s2)]), n2)
buildS n (Choice [])   = buildS n Empty
buildS n (Choice [sr]) = buildS n sr
buildS n (Choice srs)  =
  let (frags, n1) = buildAll n srs
      ns = n1; na = n1 + 1
      ins  = [(ns, Eps, s) | (s, _, _) <- frags]
      outs = [(a, Eps, na) | (_, a, _) <- frags]
      ts   = concatMap (\(_, _, t) -> t) frags
  in ((ns, na, ts ++ ins ++ outs), n1 + 2)

buildAll :: Int -> [SRegExp] -> ([Frag], Int)
buildAll n []       = ([], n)
buildAll n (sr:srs) =
  let (frag, n')    = buildS n sr
      (rest, nFinal) = buildAll n' srs
  in (frag : rest, nFinal)
