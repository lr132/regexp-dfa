module Types where

import qualified Data.Map.Strict as M

data LRegExp = Let [(Int,RegExp)] RegExp
  deriving (Eq, Show, Read)

data RegExp = S SRegExp | Star SRegExp | Plus SRegExp | Seq [RegExp] | AltR [RegExp] | StarR RegExp | PlusR RegExp
  deriving (Eq, Show, Read)

data SRegExp
  = Empty | Lit Char | Opt Char | Any | OptAny
  | SSeq [SRegExp] | Choice [SRegExp] | Var Int
  deriving (Eq, Show, Read)

data Label = Eps | OnChar Char | OnAny
  deriving (Eq, Ord, Show)

data NFA = NFA
  { nfaStates :: [Int]
  , nfaAlpha  :: [Char]
  , nfaTrans  :: [(Int, Label, Int)]
  , nfaStart  :: Int
  , nfaAccept :: [Int]
  } deriving (Show)

data DFA = DFA
  { dfaStates :: [Int]
  , dfaAlpha  :: [Char]
  , dfaTrans  :: M.Map (Int, Char) Int
  , dfaStart  :: Int
  , dfaAccept :: [Int]
  } deriving (Show)
