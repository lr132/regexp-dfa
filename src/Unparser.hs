module Unparser (unparseRegExp) where

import Types
import Data.List (intercalate)

unparseRegExp :: LRegExp -> String
unparseRegExp (Let [] e) = unparseR e
unparseRegExp (Let bs e) = concatMap bindStr (reverse bs) ++ " in " ++ unparseR e
  where
    bindStr (v, re) = "let x" ++ show v ++ " = " ++ unparseR re

unparseR :: RegExp -> String
unparseR (S sr)    = unparseS sr
unparseR (Star sr) = unparseUnit sr ++ "*"
unparseR (Plus sr) = unparseUnit sr ++ "+"
unparseR (Seq rs)  = concatMap unparseRInSeq rs
unparseR (AltR rs) = foldr1 (\a b -> a ++ "|" ++ b) (map unparseR rs)
unparseR (StarR r) = "(" ++ unparseR r ++ ")*"
unparseR (PlusR r) = "(" ++ unparseR r ++ ")+"

unparseS :: SRegExp -> String
unparseS Empty       = "e"
unparseS (Lit c)     = [c]
unparseS (Opt c)     = [c, '?']
unparseS Any         = "."
unparseS OptAny      = ".?"
unparseS (Var n)     = "x" ++ show n
unparseS (SSeq srs)  = concatMap unparseSInSeq srs
unparseS (Choice cs) = intercalate "|" (map unparseS cs)

unparseRInSeq :: RegExp -> String
unparseRInSeq (S c@(Choice _)) = "(" ++ unparseS c ++ ")"
unparseRInSeq r                = unparseR r

unparseSInSeq :: SRegExp -> String
unparseSInSeq c@(Choice _) = "(" ++ unparseS c ++ ")"
unparseSInSeq sr           = unparseS sr

unparseUnit :: SRegExp -> String
unparseUnit sr
  | isSimple sr = unparseS sr
  | otherwise   = "(" ++ unparseS sr ++ ")"
  where
    isSimple (Lit _) = True
    isSimple (Opt _) = True
    isSimple (Var _) = True
    isSimple Any     = True
    isSimple OptAny  = True
    isSimple _       = False
