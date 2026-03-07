module Parser (parseRegExp) where

import Parsing
import Types
import Data.List (nub)

many1 :: Parser a -> Parser [a]
many1 p = do x <- p; xs <- many p; return (x:xs)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do x <- p; xs <- many (sep >> p); return (x:xs)

spaces :: Parser ()
spaces = do _ <- many (char ' '); return ()

pVarName :: Parser Int
pVarName = do
  _ <- char 'x'
  ds <- many1 digit
  return (read ds)

pLRegExp :: Parser LRegExp
pLRegExp = pLet <|> (Let [] <$> pRegExp)
  where
    pLet = do
      _ <- string "let"; spaces
      v <- pVarName; spaces
      _ <- char '='; spaces
      e <- pRegExp; spaces
      _ <- string "in"; spaces
      Let bs body <- pLRegExp
      return (Let ((v,e):bs) body)

pRegExp :: Parser RegExp
pRegExp = do
  terms <- sepBy1 pSequence (char '|')
  case terms of
    [t] -> return t
    _   -> return (AltR terms)

pSequence :: Parser RegExp
pSequence = mergeSeq <$> many1 pFactor

-- pFactor works at RegExp level so grouped expressions can be starred/plussed
pFactor :: Parser RegExp
pFactor = do
  a <- pAtomR
  (do _ <- char '*'; return (starR a))
    <|> (do _ <- char '+'; return (plusR a))
    <|> return a

-- Smart constructors: collapse when inner is already a simple SRegExp
starR :: RegExp -> RegExp
starR (S sr) = Star sr
starR r      = StarR r

plusR :: RegExp -> RegExp
plusR (S sr) = Plus sr
plusR r      = PlusR r

-- Returns a RegExp directly: parens keep their structure, simple atoms wrapped in S
pAtomR :: Parser RegExp
pAtomR = pParens <|> (S <$> pAtomSimple)

pAtomSimple :: Parser SRegExp
pAtomSimple = pVar <|> pOptAny <|> pAny <|> pOptChar <|> pLit <|> pEmpty
  where
    pVar     = Var <$> pVarName
    pOptAny  = char '.' >> char '?' >> return OptAny
    pAny     = char '.' >> return Any
    pOptChar = do c <- upper; _ <- char '?'; return (Opt c)
    pLit     = Lit <$> upper
    pEmpty   = char 'e' >> return Empty

pParens :: Parser RegExp
pParens = do
  _ <- char '('
  r <- pRegExp
  _ <- char ')'
  return r

flattenSSeq :: [SRegExp] -> SRegExp
flattenSSeq = go . concatMap step
  where
    step (SSeq xs) = concatMap step xs
    step Empty     = []
    step x         = [x]
    go []  = Empty
    go [x] = x
    go xs  = SSeq xs

mergeSeq :: [RegExp] -> RegExp
mergeSeq rs = case go rs of
    []  -> S Empty
    [r] -> r
    mrs -> Seq mrs
  where
    go [] = []
    go [r] = [r]
    go (S s1 : S s2 : rest) = go (S (flattenSSeq [s1, s2]) : rest)
    go (r : rest) = r : go rest

toSRegExp :: RegExp -> SRegExp
toSRegExp (S s)      = s
toSRegExp (Seq rs)   = flattenSSeq (map toSRegExp rs)
toSRegExp (AltR rs)  = Choice (map toSRegExp rs)
toSRegExp (Star s)   = s
toSRegExp (Plus s)   = s
toSRegExp (StarR r)  = toSRegExp r
toSRegExp (PlusR r)  = toSRegExp r

parseRegExp :: String -> Maybe LRegExp
parseRegExp s = case parse pLRegExp s of
  [(r, "")] -> Just r
  _         -> Nothing
