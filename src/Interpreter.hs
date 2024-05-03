module Interpreter where

import Algorithms (
  augment,
  columnSpace,
  columns,
  ef,
  eigenSpace,
  gramSchmidt,
  gsProj,
  independentSubset,
  inverse,
  isBasis,
  isConsistent,
  isEigenValue,
  isEigenVector,
  isLinearlyIndependent,
  nullSpace,
  nullity,
  qrAlgo,
  rank,
  rowSpace,
  rref,
  spans,
  withinSpan,
 )
import Data.Map (Map)
import qualified Data.Map as M
import Types (
  BinOp (..),
  Expression (..),
  ExpressionBlock,
  Function (..),
  Value (..),
  genMatAdd,
  matAdd,
  matMult,
  scalMatMult,
  transpose,
 )

interpret :: Map String Value -> Value -> ExpressionBlock -> (Map String Value, Value)
interpret m ans = foldr f (m, ans)
 where
  f a (m', v) = interpretExp (M.insert "ANS" v m') a

interpretExp :: Map String Value -> Expression -> (Map String Value, Value)
interpretExp m ex = case ex of
  (Assignment s e) -> interpretAssignment m s (interpretExpression m e)
  e -> (m, interpretExpression m e)

interpretExpression :: Map String Value -> Expression -> Value
interpretExpression m ex = case ex of
  (Variable s) -> interpretVariable m s
  (ValueExpression v) -> v
  (FunctionExpression f es) -> interpretFunction m f es
  (BinaryOperation b e1 e2) -> interpretBinOp b (interpretExpression m e1) (interpretExpression m e2)
  (Negation e) -> interpretNegate (interpretExpression m e)
  _ -> error "Unreachable error, assigment should be handled by wrapper function"

interpretAssignment :: Map String Value -> String -> Value -> (Map String Value, Value)
interpretAssignment m s v = (M.insert s v m, v)

interpretVariable :: Map String Value -> String -> Value
interpretVariable m s = case M.lookup s m of
  (Just v) -> v
  Nothing -> error $ "Variable " ++ s ++ " does not exist."

interpretNegate :: Value -> Value
interpretNegate val = case val of
  (S s) -> S (-s)
  (M m) -> M (scalMatMult (-1) m)
  (V v) -> V (scalMatMult (-1) v)
  (VL vl) -> VL (map (scalMatMult (-1)) vl)
  _ -> error "Booleans cannot be negated"

interpretBinOp :: BinOp -> Value -> Value -> Value
interpretBinOp b val1 val2 = case (b, val1, val2) of
  (_, VL _, _) -> error "no binary operations on vector sets"
  (_, _, VL _) -> error "no binary operations on vector sets"
  (Add, S s1, S s2) -> S (s1 + s2)
  (Add, M m1, M m2) -> M (matAdd m1 m2)
  (Add, V v1, V v2) -> V (matAdd v1 v2)
  (Add, V v1, M m2) -> M (matAdd v1 m2)
  (Add, M m1, V v2) -> V (matAdd m1 v2)
  (Add, S _, _) -> error "Scalars cannot be added to matrices"
  (Add, _, S _) -> error "Scalars cannot be added to matrices"
  (Sub, S s1, S s2) -> S (s1 - s2)
  (Sub, M m1, M m2) -> M (genMatAdd (-) m1 m2)
  (Sub, V v1, V v2) -> V (genMatAdd (-) v1 v2)
  (Sub, M m1, V v2) -> M (genMatAdd (-) m1 v2)
  (Sub, V v1, M m2) -> V (genMatAdd (-) v1 m2)
  (Sub, S _, _) -> error "Matrices cannot be subtracted from scalars"
  (Sub, _, S _) -> error "Scalars cannot be subtracted from matrices"
  (Mul, S s1, S s2) -> S (s1 * s2)
  (Mul, M m1, M m2) -> M (matMult m1 m2)
  (Mul, V v1, V v2) -> V (matMult v1 v2)
  (Mul, M m1, V v2) -> M (matMult m1 v2)
  (Mul, V v1, M m2) -> M (matMult v1 m2)
  (Mul, S s1, M m2) -> M (scalMatMult s1 m2)
  (Mul, M m1, S s2) -> M (scalMatMult s2 m1)
  (Mul, S s1, V v2) -> V (scalMatMult s1 v2)
  (Mul, V v1, S s2) -> V (scalMatMult s2 v1)
  (Div, S s1, S s2) -> S (s1 / s2)
  (Div, _, _) -> error "Only scalars can be divided"
  _ -> error "Booleans cannot be operated on"

interpretFunction :: Map String Value -> Function -> ExpressionBlock -> Value
interpretFunction m f es = case f of
  INVERSE -> case head vs of
    (M mat) -> M $ inverse mat
    _ -> error "Incorrect type for inverse"
  RREF -> case head vs of
    (M mat) -> (M . snd . rref) mat
    _ -> error "Incorrect type for rref"
  EF -> case head vs of
    (M mat) -> (M . snd . ef) mat
    _ -> error "Incorrect type for ef"
  SPAN -> case head vs of
    (M mat) -> (VL . independentSubset . columns) mat
    _ -> error "Incorrect type for span"
  DETERMINANT -> case head vs of
    (M mat) -> case ef mat of
      (Just d, _) -> S d
      _ -> error "Matrix does not have a determinant"
    _ -> error "Incorrect type for determinant"
  PROJECT -> case (head vs, (head . tail) vs) of
    (V v1, V v2) -> M $ gsProj v1 v2
    _ -> error "Incorrect type for project"
  DIM -> case head vs of
    (VL vl) -> (S . toRational . length . independentSubset) vl
    _ -> error "Incorrect type for dimension"
  RANK -> case head vs of
    (M mat) -> (S . toRational . rank) mat
    _ -> error "Incorrect type for rank"
  NULLITY -> case head vs of
    (M mat) -> (S . toRational . nullity) mat
    _ -> error "Incorrect type for nullity"
  IS_CONSISTENT -> case head vs of
    (M mat) -> (B . isConsistent) mat
    _ -> error "Incorrect type for consistency checking"
  COL -> case head vs of
    (M mat) -> (VL . columnSpace) mat
    _ -> error "Incorrect type for COL"
  ROW -> case head vs of
    (M mat) -> (VL . rowSpace) mat
    _ -> error "Incorrect type for ROW"
  NUL -> case head vs of
    (M mat) -> (VL . nullSpace) mat
    _ -> error "Incorrect type for NUL"
  SPANS -> case (head vs, (head . tail) vs) of
    (VL vl1, VL vl2) -> B $ spans vl1 vl2
    _ -> error "Incorrect type for spans"
  IS_BASIS -> case head vs of
    (VL vl) -> B $ isBasis vl
    _ -> error "Incorrect type for is basis"
  QR -> case head vs of
    (M mat) -> M $ qrAlgo mat
    _ -> error "Incorrect type for QR"
  AUGMENT -> case (head vs, (head . tail) vs) of
    (M m1, M m2) -> M $ augment m1 m2
    (V v1, V v2) -> M $ augment v1 v2
    (V v1, M m2) -> M $ augment v1 m2
    (M m1, V v2) -> M $ augment m1 v2
    _ -> error "Incorrect type for augment"
  TRANSPOSE -> case head vs of
    (M mat) -> M $ transpose mat
    _ -> error "Incorrect type for transpose"
  ORTHO_BASIS -> case head vs of
    (M mat) -> M $ gramSchmidt mat
    _ -> error "Incorrect type for orthonormal basis"
  IN_SPAN -> case (head vs, (head . tail) vs) of
    (VL vl, V v) -> B $ withinSpan vl v
    _ -> error "Incorrect type for in span"
  IS_INDEPENDENT -> case head vs of
    (VL vl) -> B $ isLinearlyIndependent vl
    _ -> error "Incorrect type for independent check"
  EIGENSPACE -> case (head vs, (head . tail) vs) of
    (M mat, S s) -> VL $ eigenSpace mat s
    _ -> error "Incorrect type for in eigenspace"
  IS_EIGENVALUE -> case (head vs, (head . tail) vs) of
    (M mat, S s) -> B $ isEigenValue mat s
    _ -> error "Incorrect type for in is eigenvalue"
  IS_EIGENVECTOR -> case (head vs, (head . tail) vs) of
    (V v, M mat) -> B $ isEigenVector v mat
    _ -> error "Incorrect type for in span"
 where
  vs = map (interpretExpression m) es