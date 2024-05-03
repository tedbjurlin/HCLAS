module Interpreter where

import Algorithms
import Data.Map (Map)
import qualified Data.Map as M
import Types

interpret :: Map String Value -> Value -> ExpressionBlock -> (Map String Value, Value)
interpret m ans = foldr f (m, ans)
 where
  f a (m', v) = interpretExpression (M.insert "ANS" v m') a

interpretExpression :: Map String Value -> Expression -> (Map String Value, Value)
interpretExpression m ex = case ex of
  (Assignment s e) -> interpretAssignment m s ((snd . interpretExpression m) e)
  (Variable s) -> interpretVariable m s
  (ValueExpression v) -> (m, v)
  (FunctionExpression f es) -> interpretFunction m f es
  (BinaryOperation b e1 e2) -> (m, interpretBinOp b ((snd . interpretExpression m) e1) ((snd . interpretExpression m) e2))
  (Negation e) -> (m, interpretNegate ((snd . interpretExpression m) e))

interpretAssignment :: Map String Value -> String -> Value -> (Map String Value, Value)
interpretAssignment m s v = (M.insert s v m, v)

interpretVariable :: Map String Value -> String -> (Map String Value, Value)
interpretVariable m s = case M.lookup s m of
  (Just v) -> (m, v)
  Nothing -> error $ "Variable " ++ s ++ " does not exist."

interpretNegate :: Value -> Value
interpretNegate val = case val of
  (S s) -> S (-s)
  (M m) -> M (scalMatMult (-1) m)
  (V v) -> V (scalMatMult (-1) v)
  (VL vl) -> VL (map (scalMatMult (-1)) vl)

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

interpretFunction :: Map String Value -> Function -> ExpressionBlock -> (Map String Value, Value)
interpretFunction m f es = case f of
  INVERSE -> case head vs of
    (M mat) -> (m, inverse mat)
    _ -> error "Incorrect type for inverse"
 where
  vs = map (interpretExpression m) es