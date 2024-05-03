{-# LANGUAGE TupleSections #-}

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
import Data.Foldable (foldrM)
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

interpret :: Map String Value -> Value -> ExpressionBlock -> Either String (Map String Value, Value)
interpret m ans = foldrM f (m, ans)
 where
  f a (m', v) = interpretExp (M.insert "ANS" v m') a

interpretExp :: Map String Value -> Expression -> Either String (Map String Value, Value)
interpretExp m ex = case ex of
  (Assignment s e) -> (Right . interpretAssignment m s) =<< interpretExpression m e
  e -> (Right . (m,)) =<< interpretExpression m e

interpretExpression :: Map String Value -> Expression -> Either String Value
interpretExpression m ex = case ex of
  (Variable s) -> interpretVariable m s
  (ValueExpression v) -> Right v
  (FunctionExpression f es) -> interpretFunction m f es
  (BinaryOperation b e1 e2) -> interpretBinOp b (interpretExpression m e1) (interpretExpression m e2)
  (Negation e) -> interpretNegate =<< interpretExpression m e
  _ -> Left "Unreachable Left, assigment should be handled by wrapper function"

interpretAssignment :: Map String Value -> String -> Value -> (Map String Value, Value)
interpretAssignment m s v = (M.insert s v m, v)

interpretVariable :: Map String Value -> String -> Either String Value
interpretVariable m s = case M.lookup s m of
  (Just v) -> Right v
  Nothing -> Left $ "Variable " ++ s ++ " does not exist."

interpretNegate :: Value -> Either String Value
interpretNegate val = case val of
  (S s) -> Right $ S (-s)
  (M m) -> Right $ M (scalMatMult (-1) m)
  (V v) -> Right $ V (scalMatMult (-1) v)
  (VL vl) -> Right $ VL (map (scalMatMult (-1)) vl)
  _ -> Left "Booleans cannot be negated"

interpretBinOp :: BinOp -> Either String Value -> Either String Value -> Either String Value
interpretBinOp b e1 e2 = do
  val1 <- e1
  val2 <- e2
  case (b, val1, val2) of
    (_, VL _, _) -> Left "no binary operations on vector sets"
    (_, _, VL _) -> Left "no binary operations on vector sets"
    (Add, S s1, S s2) -> Right $ S (s1 + s2)
    (Add, M m1, M m2) -> (Right . M) =<< matAdd m1 m2
    (Add, V v1, V v2) -> (Right . V) =<< matAdd v1 v2
    (Add, V v1, M m2) -> (Right . M) =<< matAdd v1 m2
    (Add, M m1, V v2) -> (Right . V) =<< matAdd m1 v2
    (Add, S _, _) -> Left "Scalars cannot be added to matrices"
    (Add, _, S _) -> Left "Scalars cannot be added to matrices"
    (Sub, S s1, S s2) -> Right $ S (s1 - s2)
    (Sub, M m1, M m2) -> (Right . M) =<< genMatAdd (-) m1 m2
    (Sub, V v1, V v2) -> (Right . V) =<< genMatAdd (-) v1 v2
    (Sub, M m1, V v2) -> (Right . M) =<< genMatAdd (-) m1 v2
    (Sub, V v1, M m2) -> (Right . V) =<< genMatAdd (-) v1 m2
    (Sub, S _, _) -> Left "Matrices cannot be subtracted from scalars"
    (Sub, _, S _) -> Left "Scalars cannot be subtracted from matrices"
    (Mul, S s1, S s2) -> Right $ S (s1 * s2)
    (Mul, M m1, M m2) -> (Right . M) =<< matMult m1 m2
    (Mul, V v1, V v2) -> (Right . V) =<< matMult v1 v2
    (Mul, M m1, V v2) -> (Right . M) =<< matMult m1 v2
    (Mul, V v1, M m2) -> (Right . M) =<< matMult v1 m2
    (Mul, S s1, M m2) -> Right $ M (scalMatMult s1 m2)
    (Mul, M m1, S s2) -> Right $ M (scalMatMult s2 m1)
    (Mul, S s1, V v2) -> Right $ V (scalMatMult s1 v2)
    (Mul, V v1, S s2) -> Right $ V (scalMatMult s2 v1)
    (Div, S s1, S s2) ->
      if s2 == 0
        then Left "Divide by zero error"
        else Right $ S (s1 / s2)
    (Div, _, _) -> Left "Only scalars can be divided"
    _ -> Left "Booleans cannot be operated on"

interpretFunction :: Map String Value -> Function -> ExpressionBlock -> Either String Value
interpretFunction m f es = do
  vs <- mapM (interpretExpression m) es
  case f of
    INVERSE -> case head vs of
      (M mat) -> Right $ M $ inverse mat
      _ -> Left "Incorrect type for inverse"
    RREF -> case head vs of
      (M mat) -> (Right . M . snd . rref) mat
      _ -> Left "Incorrect type for rref"
    EF -> case head vs of
      (M mat) -> (Right . M . snd . ef) mat
      _ -> Left "Incorrect type for ef"
    SPAN -> case head vs of
      (M mat) -> (Right . VL . independentSubset . columns) mat
      _ -> Left "Incorrect type for span"
    DETERMINANT -> case head vs of
      (M mat) -> case ef mat of
        (Just d, _) -> Right $ S d
        _ -> Left "Matrix does not have a determinant"
      _ -> Left "Incorrect type for determinant"
    PROJECT -> case (head vs, (head . tail) vs) of
      (V v1, V v2) -> Right $ M $ gsProj v1 v2
      _ -> Left "Incorrect type for project"
    DIM -> case head vs of
      (VL vl) -> (Right . S . toRational . length . independentSubset) vl
      _ -> Left "Incorrect type for dimension"
    RANK -> case head vs of
      (M mat) -> (Right . S . toRational . rank) mat
      _ -> Left "Incorrect type for rank"
    NULLITY -> case head vs of
      (M mat) -> (Right . S . toRational . nullity) mat
      _ -> Left "Incorrect type for nullity"
    IS_CONSISTENT -> case head vs of
      (M mat) -> (Right . B . isConsistent) mat
      _ -> Left "Incorrect type for consistency checking"
    COL -> case head vs of
      (M mat) -> (Right . VL . columnSpace) mat
      _ -> Left "Incorrect type for COL"
    ROW -> case head vs of
      (M mat) -> (Right . VL . rowSpace) mat
      _ -> Left "Incorrect type for ROW"
    NUL -> case head vs of
      (M mat) -> (Right . VL . nullSpace) mat
      _ -> Left "Incorrect type for NUL"
    SPANS -> case (head vs, (head . tail) vs) of
      (VL vl1, VL vl2) -> Right $ B $ spans vl1 vl2
      _ -> Left "Incorrect type for spans"
    IS_BASIS -> case head vs of
      (VL vl) -> Right $ B $ isBasis vl
      _ -> Left "Incorrect type for is basis"
    QR -> case head vs of
      (M mat) -> (Right . M) =<< qrAlgo mat
      _ -> Left "Incorrect type for QR"
    AUGMENT -> case (head vs, (head . tail) vs) of
      (M m1, M m2) -> Right $ M $ augment m1 m2
      (V v1, V v2) -> Right $ M $ augment v1 v2
      (V v1, M m2) -> Right $ M $ augment v1 m2
      (M m1, V v2) -> Right $ M $ augment m1 v2
      _ -> Left "Incorrect type for augment"
    TRANSPOSE -> case head vs of
      (M mat) -> Right $ M $ transpose mat
      _ -> Left "Incorrect type for transpose"
    ORTHO_BASIS -> case head vs of
      (M mat) -> (Right . M) =<< gramSchmidt mat
      _ -> Left "Incorrect type for orthonormal basis"
    IN_SPAN -> case (head vs, (head . tail) vs) of
      (VL vl, V v) -> Right $ B $ withinSpan vl v
      _ -> Left "Incorrect type for in span"
    IS_INDEPENDENT -> case head vs of
      (VL vl) -> (Right . B . isLinearlyIndependent) vl
      _ -> Left "Incorrect type for independent check"
    EIGENSPACE -> case (head vs, (head . tail) vs) of
      (M mat, S s) -> (Right . VL) =<< eigenSpace mat s
      _ -> Left "Incorrect type for in eigenspace"
    IS_EIGENVALUE -> case (head vs, (head . tail) vs) of
      (M mat, S s) -> (Right . B) =<< isEigenValue mat s
      _ -> Left "Incorrect type for in is eigenvalue"
    IS_EIGENVECTOR -> case (head vs, (head . tail) vs) of
      (V v, M mat) -> (Right . B) =<< isEigenVector v mat
      _ -> Left "Incorrect type for in span"