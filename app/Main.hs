module Main where

import Types
import Parser      (parseRegExp)
import Unparser    (unparseRegExp)
import NFA         (toNFA, extractAlpha, inlineLet)
import DFA         (toDFA)
import Visualise   (toDot, toAsciiTable)
import DFFile      (serialiseDF, deserialiseDF)
import BoundedLang (boundedLang)
import Matcher     (matchesRegex)

import System.IO   (hSetBuffering, stdout, BufferMode(..))
import System.Exit (exitSuccess)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "RegexpDFA - Regexp to DFA converter"
  putStrLn "Type a regex, or 'help' for commands.\n"
  repl

repl :: IO ()
repl = do
  putStr "> "
  line <- getLine
  case words line of
    []                    -> repl
    ["quit"]              -> exitSuccess
    ["exit"]              -> exitSuccess
    ["help"]              -> printHelp >> repl
    ("test":str:rest)     -> cmdTest str (unwords rest) >> repl
    ("examples":n:rest)   -> cmdExamples (read n) (unwords rest) >> repl
    ("export":file:rest)  -> cmdExport file (unwords rest) >> repl
    ("load":file:_)       -> cmdLoad file >> repl
    ("dot":rest)          -> cmdDot (unwords rest) >> repl
    _                     -> cmdCompile line >> repl

-- Compile a regex string and display the DFA
cmdCompile :: String -> IO ()
cmdCompile input = case compile input of
  Left err        -> putStrLn $ "Error: " ++ err
  Right (re, dfa) -> do
    putStrLn $ "\nRegexp:   " ++ input
    putStrLn $ "Alphabet: {" ++ commas (map (:[]) (dfaAlpha dfa)) ++ "}"
    putStrLn $ "States:   " ++ show (length (dfaStates dfa))
              ++ "  |  Start: " ++ show (dfaStart dfa)
              ++ "  |  Accepting: {" ++ commas (map show (dfaAccept dfa)) ++ "}"
    putStrLn ""
    putStr (toAsciiTable dfa)
    let exs = take 8 (boundedLang 6 re)
    if null exs
      then putStrLn "No matches up to length 6."
      else putStrLn $ "Examples: " ++ commas (map show exs)
    putStrLn "\nDOT (paste into graphviz.online or Graphviz Online):"
    putStr (toDot dfa)

-- Test a string against a regex
cmdTest :: String -> String -> IO ()
cmdTest str input = case parseRegExp (filter (/= ' ') input) of
  Nothing -> putStrLn "Error: could not parse regex"
  Just re -> do
    let result = matchesRegex str re
    putStrLn $ show str ++ if result then " matches." else " does not match."

-- Show example matches up to length n
cmdExamples :: Int -> String -> IO ()
cmdExamples n input = case parseRegExp (filter (/= ' ') input) of
  Nothing -> putStrLn "Error: could not parse regex"
  Just re -> do
    let exs = boundedLang n re
    if null exs
      then putStrLn $ "No matches up to length " ++ show n ++ "."
      else mapM_ (putStrLn . show) exs

-- Export to .df file
cmdExport :: FilePath -> String -> IO ()
cmdExport file input = case compile input of
  Left err        -> putStrLn $ "Error: " ++ err
  Right (_, dfa)  -> do
    writeFile file (serialiseDF input dfa)
    putStrLn $ "Saved to " ++ file

-- Load a .df file
cmdLoad :: FilePath -> IO ()
cmdLoad file = do
  content <- readFile file
  case deserialiseDF content of
    Left err        -> putStrLn $ "Error: " ++ err
    Right (re, dfa) -> do
      putStrLn $ "Loaded: " ++ re
      putStrLn $ "States: " ++ show (length (dfaStates dfa))
              ++ "  |  Start: " ++ show (dfaStart dfa)
              ++ "  |  Accepting: {" ++ commas (map show (dfaAccept dfa)) ++ "}"
      putStrLn ""
      putStr (toAsciiTable dfa)
      putStr (toDot dfa)

-- Print DOT only
cmdDot :: String -> IO ()
cmdDot input = case compile input of
  Left err       -> putStrLn $ "Error: " ++ err
  Right (_, dfa) -> putStr (toDot dfa)

-- Core pipeline: parse -> inline -> NFA -> DFA
compile :: String -> Either String (LRegExp, DFA)
compile input = do
  let stripped = filter (/= ' ') input
  lre <- maybe (Left "Parse error: invalid regex") Right (parseRegExp stripped)
  re  <- inlineLet lre
  let alpha = if null (extractAlpha lre) then ['A'..'Z'] else extractAlpha lre
      nfa   = toNFA alpha re
      dfa   = toDFA nfa
  Right (lre, dfa)

printHelp :: IO ()
printHelp = putStrLn $ unlines
  [ "Commands:"
  , "  <regex>                 compile regex and show DFA"
  , "  test \"<str>\" <regex>    test string against regex"
  , "  examples <n> <regex>    show matches up to length n"
  , "  export <file> <regex>   save .df file"
  , "  load <file>             load and display .df file"
  , "  dot <regex>             print DOT graph only"
  , "  help                    show this message"
  , "  quit                    exit"
  ]

commas :: [String] -> String
commas = foldr1 (\a b -> a ++ ", " ++ b) . \xs -> if null xs then [""] else xs
