{-# LANGUAGE GADTs #-}

module Main where

import Data.List.NonEmpty as NE (head)
import Data.Map (Map, empty)
import Interpreter (interpret)
import Parser (parseInput)
import PrettyPrinter (printPretty)
import System.IO (hFlush, stdout)
import Text.Megaparsec (ErrorItem (EndOfInput), ParseError (TrivialError), ParseErrorBundle (bundleErrors), errorBundlePretty)
import Types (Function (INVERSE), Value (S))

main :: IO ()
main = do
  putStrLn "--------------------"
  putStrLn "Welcome to CLAS v1.0"
  putStrLn "--------------------"
  putStrLn "Syntax:"
  putStrLn "\t`name <- expression` saves expression to variable"
  putStrLn "\t`[1 2 3 | 4 5 6]` is a 2x3 matrix"
  putStrLn "\t`<1 2 3>` is a 3-dimensional vector"
  putStrLn "\t`RREF(A)` calculates reduced echelon form of A"
  putStrLn "\t`A * B` multiplies matrices or vectors A and B (vector multiplication uses the dot product)"
  putStrLn "Enjoy!"
  repl empty (S 0)

repl :: Map String Value -> Value -> IO ()
repl m v = do
  putStr ">> "
  hFlush stdout
  l <- getLine
  replBody l m v

multiLine :: String -> Map String Value -> Value -> IO ()
multiLine l m v = do
  putStr ".. "
  hFlush stdout
  l2 <- getLine
  let l' = l ++ l2
  replBody l' m v

replBody :: String -> Map String Value -> Value -> IO ()
replBody l m v
  | l == "EXIT()" = return ()
  | l == "HELP" = putStrLn ("\nAvailable Functions:\n\n" ++ (unlines . map show . enumFrom) INVERSE) >> repl m v
  | otherwise = case parseInput l of
      (Left err) -> do
        let errs = bundleErrors err
        let e = NE.head errs
        case e of
          (TrivialError _ (Just EndOfInput) _) -> multiLine l m v
          _ -> putStrLn (errorBundlePretty err) >> repl m v
      (Right ex) -> do
        case interpret m v ex of
          Left err -> do
            putStrLn err
            repl m v
          Right (m', v') -> do
            printPretty v'
            repl m' v'