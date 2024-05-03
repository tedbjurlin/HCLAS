module PrettyPrinter where

import Data.Array (bounds, elems)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Ratio (denominator, numerator)
import Text.PrettyPrint.Boxes (Box, center1, char, hsep, left, render, right, text, top, vcat, (//), (<+>))
import Types (Matrix, Scalar, Value (B, M, S, V, VL), Vector)

prettyColumn :: [Scalar] -> Box
prettyColumn ss = nums <+> seps <+> denoms
 where
  nums = vcat right $ map (text . show . numerator) ss
  denoms = vcat right $ map (text . show . denominator) ss
  seps = vcat center1 $ replicate (length ss) (char '/')

prettyMatrix :: Matrix -> Box
prettyMatrix m = hsep 2 top [front, ss, back]
 where
  c = (snd . snd . bounds) m
  es = transpose $ chunksOf c $ elems m
  ss = hsep 2 top $ map prettyColumn es
  border ch = vcat left $ replicate (length es - 1) (char ch)
  front = char '[' // border '|'
  back = border ' ' // char ']'

prettyVector :: Vector -> Box
prettyVector v = hsep 2 top [front, ss, back]
 where
  es = elems v
  ss = prettyColumn es
  spaces = vcat left $ replicate (length es - 1) (char ' ')
  front = char '<' // spaces
  back = spaces // char '>'

prettyScalar :: Scalar -> Box
prettyScalar s = text ((show . numerator) s) <+> char '/' <+> text ((show . denominator) s)

prettyVectorList :: [Vector] -> Box
prettyVectorList vs = hsep 2 top [front, inner '<', ss, inner '>', back]
 where
  es = transpose $ map elems vs
  ss = hsep 2 top $ map prettyColumn es
  border ch = vcat left $ replicate (length es - 1) (char ch)
  front = char '{' // border ','
  back = border ' ' // char '}'
  inner ch = vcat left $ replicate (length es) (char ch)

prettyBoolean :: Bool -> Box
prettyBoolean True = text "YES"
prettyBoolean False = text "NO"

printPretty :: Value -> IO ()
printPretty v = putStrLn $ render $ case v of
  (B b) -> prettyBoolean b
  (M m) -> prettyMatrix m
  (V vec) -> prettyVector vec
  (VL vl) -> prettyVectorList vl
  (S s) -> prettyScalar s