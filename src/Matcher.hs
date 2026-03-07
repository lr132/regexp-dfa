module Matcher (matchesRegex) where

import Types
import qualified Data.Map.Strict as M

matchesRegex :: String -> LRegExp -> Bool
matchesRegex str (Let bs e) = matchR str e
  where
    env :: M.Map Int RegExp
    env = M.fromList bs

    matchR :: String -> RegExp -> Bool
    matchR s (S sr)     = matchS s sr
    matchR _ (Star Any) = True
    matchR s (Star sr)  = any (\n -> matchRep n s sr) [0..length s]
    matchR s (Plus Any) = not (null s)
    matchR s (Plus sr)  = any (\n -> matchRep n s sr) [1..length s]
    matchR s (Seq rs)   = matchSeq s rs
    matchR s (AltR rs)  = any (matchR s) rs
    matchR s (StarR r)  = any (\n -> matchRepR n s r) [0..length s]
    matchR s (PlusR r)  = any (\n -> matchRepR n s r) [1..length s]

    matchRep :: Int -> String -> SRegExp -> Bool
    matchRep 0 s _  = null s
    matchRep n s sr = any tryPrefix [1..length s]
      where tryPrefix i = matchS (take i s) sr && matchRep (n-1) (drop i s) sr

    matchRepR :: Int -> String -> RegExp -> Bool
    matchRepR 0 s _  = null s
    matchRepR n s r  = any tryPrefix [1..length s]
      where tryPrefix i = matchR (take i s) r && matchRepR (n-1) (drop i s) r

    matchSeq :: String -> [RegExp] -> Bool
    matchSeq s []     = null s
    matchSeq s (r:rs) = any trySplit [0..length s]
      where trySplit i = matchR (take i s) r && matchSeq (drop i s) rs

    matchS :: String -> SRegExp -> Bool
    matchS s (SSeq srs)   = matchSSeq s srs
    matchS s (Choice srs) = any (matchS s) srs
    matchS s (Var v)      = maybe False (matchR s) (M.lookup v env)
    matchS s sr           = matchAtom s sr

    matchSSeq :: String -> [SRegExp] -> Bool
    matchSSeq s []       = null s
    matchSSeq s (sr:srs) = any trySplit [0..length s]
      where trySplit i = matchS (take i s) sr && matchSSeq (drop i s) srs

    matchAtom :: String -> SRegExp -> Bool
    matchAtom "" Empty   = True
    matchAtom "" (Opt _) = True
    matchAtom "" OptAny  = True
    matchAtom [_] Any    = True
    matchAtom [_] OptAny = True
    matchAtom [c] (Lit c') = c == c'
    matchAtom [c] (Opt c') = c == c'
    matchAtom _ _          = False
