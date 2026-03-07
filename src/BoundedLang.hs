module BoundedLang (boundedLang) where

import Types
import Data.List (nub, sort)
import qualified Data.Map.Strict as M

boundedLang :: Int -> LRegExp -> [String]
boundedLang maxLen (Let bindings expr) = sort $ gen maxLen expr
  where
    env :: M.Map Int RegExp
    env = M.fromList bindings

    gen :: Int -> RegExp -> [String]
    gen limit _ | limit < 0 = []
    gen limit (S sr)    = genS limit sr
    gen limit (Star sr) = star limit (genS limit sr)
    gen limit (Plus sr) = plus limit (genS limit sr)
    gen limit (Seq rs)   = foldr (seqWith limit) [""] rs
    gen limit (AltR rs)  = nub $ concatMap (gen limit) rs
    gen limit (StarR r)  = star limit (gen limit r)
    gen limit (PlusR r)  = plus limit (gen limit r)

    genS :: Int -> SRegExp -> [String]
    genS limit _ | limit < 0 = []
    genS _ Empty             = [""]
    genS limit (Lit c)
      | limit >= 1           = [[c]]
      | otherwise            = []
    genS limit (Opt c)
      | limit >= 1           = ["", [c]]
      | otherwise            = [""]
    genS limit Any
      | limit >= 1           = ["."]
      | otherwise            = []
    genS limit OptAny
      | limit >= 1           = ["", "."]
      | otherwise            = [""]
    genS _ (SSeq [])         = [""]
    genS limit (SSeq (r:rs)) =
      [s1 ++ s2 | s1 <- genS limit r, s2 <- genS (limit - length s1) (SSeq rs)]
    genS limit (Choice rs)   = concatMap (genS limit) rs
    genS limit (Var v)       = maybe [] (gen limit) (M.lookup v env)

    star :: Int -> [String] -> [String]
    star limit base = "" : go [""]
      where
        nonEmpty = filter (not . null) base
        go prev
          | null next = []
          | otherwise = next ++ go next
          where
            next = [s1 ++ s2 | s1 <- nonEmpty, s2 <- prev, length s1 + length s2 <= limit]

    plus :: Int -> [String] -> [String]
    plus limit base =
      [s1 ++ s2 | s1 <- nonEmpty, s2 <- star (limit - length s1) base, length s1 + length s2 <= limit]
      where nonEmpty = filter (not . null) base

    seqWith :: Int -> RegExp -> [String] -> [String]
    seqWith limit r acc = [s1 ++ s2 | s2 <- acc, s1 <- gen (limit - length s2) r]
