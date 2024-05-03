{-# LANGUAGE GADTs #-}

module Main where

import Data.Map (Map, empty)
import Interpreter (interpret)
import Parser (parseInput)
import PrettyPrinter (printPretty)
import Types

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
  l <- readLn
  if l == "EXIT()"
    then return ()
    else case parseInput l of
      (Left err) -> putStrLn err >> repl m v
      (Right exp) -> do
        let (m', v') = interpret m v exp
        printPretty v'
        repl m' v'