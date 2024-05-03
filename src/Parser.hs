module Parser where

import Control.Monad.Combinators.Expr (
  Operator (InfixL, Prefix),
  makeExprParser,
 )
import Data.Array (listArray)
import Data.Ratio ((%))
import Data.Void (Void)
import Text.Megaparsec as P (ParseErrorBundle, Parsec, between, choice, empty, eof, many, parse, sepBy, sepBy1, some, try)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Types (
  BinOp (..),
  Expression (..),
  ExpressionBlock,
  Function (INVERSE),
  Matrix,
  Scalar,
  Value (M, S, V, VL),
  Vector,
 )

type Parser = Parsec Void String

whitespace :: Parser ()
whitespace =
  L.space
    space1
    P.empty
    P.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

reserved :: String -> Parser String
reserved = lexeme . P.try . string

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Float
float = lexeme L.float

symbol :: String -> Parser String
symbol = L.symbol whitespace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme . try $ (:) <$> letterChar <*> many alphaNumChar

pExpressionBlock :: Parser ExpressionBlock
pExpressionBlock = sepBy pExp (symbol ";")

pExp :: Parser Expression
pExp = choice [pAssignment, pExpression]

pAssignment :: Parser Expression
pAssignment = try (Assignment <$> pVarName <* reserved "<-") <*> pExpression

pVarName :: Parser String
pVarName = (lexeme . try) ((:) <$> letterChar <*> many alphaNumChar)

pExpression :: Parser Expression
pExpression = makeExprParser pAtom operatorTable

pAtom :: Parser Expression
pAtom =
  choice
    [ pValueExpression
    , pFunction
    , pVariable
    ]

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [
    [ prefix "-" Negation
    ]
  ,
    [ binary "*" Mul
    , binary "/" Div
    ]
  ,
    [ binary "+" Add
    , binary "-" Sub
    ]
  ]

binary :: String -> BinOp -> Operator Parser Expression
binary name f = InfixL (BinaryOperation f <$ symbol name)

prefix :: String -> (Expression -> Expression) -> Operator Parser Expression
prefix name f = Prefix (f <$ symbol name)

pVariable :: Parser Expression
pVariable = Variable <$> identifier

pFunction :: Parser Expression
pFunction =
  FunctionExpression
    <$> choice (map pFunctionName (enumFrom INVERSE))
    <*> pArgs

pFunctionName :: Function -> Parser Function
pFunctionName f = f <$ reserved (show f)

pArgs :: Parser ExpressionBlock
pArgs = parens (sepBy pExpression (symbol ","))

pValueExpression :: Parser Expression
pValueExpression = ValueExpression <$> pValue

pValue :: Parser Value
pValue =
  choice
    [ S <$> pScalar
    , V <$> pVector
    , M <$> pMatrix
    , VL <$> pVectorList
    ]

pVector :: Parser Vector
pVector = constructVector =<< between (symbol "<") (symbol ">") pRow

constructVector :: [Scalar] -> Parser Vector
constructVector s = return $ listArray ((1, 1), (length s, 1)) s

pVectorList :: Parser [Vector]
pVectorList = between (symbol "{") (symbol "}") (sepBy1 pVector (symbol ","))

pMatrix :: Parser Matrix
pMatrix = constructMatrix =<< between (symbol "[") (symbol "]") (sepBy1 pRow (symbol "|"))

constructMatrix :: [[Scalar]] -> Parser Matrix
constructMatrix l =
  if all ((== (length $ head l)) . length) (tail l)
    then return $ listArray ((1, 1), (length l, (length . head) l)) (concat l)
    else fail "All rows in a matrix must be of equal length."

pRow :: Parser [Scalar]
pRow = some pScalar

pScalar :: Parser Scalar
pScalar =
  choice
    [ try (mkRatio (L.signed whitespace integer <* symbol "/") integer)
    , fromIntegral <$> L.signed whitespace integer
    ]

mkRatio :: Parser Integer -> Parser Integer -> Parser Scalar
mkRatio pn pd = do
  n <- pn
  d <- pd
  if d == 0
    then fail "Divide by zero error"
    else return $ n % d

pInput :: Parser ExpressionBlock
pInput = whitespace *> pExpressionBlock <* eof

parseInput :: String -> Either (ParseErrorBundle String Void) ExpressionBlock
parseInput = parse pInput ""