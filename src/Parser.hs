module Parser where

import           Data.Void (Void)
import           Text.Megaparsec as P (Parsec, empty, try, (<|>), eof, many, parse, errorBundlePretty, choice, sepBy, between, sepBy1, some)
import           Text.Megaparsec.Char (string, space1, letterChar, char, digitChar, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L
import           System.Environment (getArgs)
import           System.FilePath.Posix (takeBaseName, (</>), (-<.>), (<.>), takeDirectory)
import           System.Directory (listDirectory, doesFileExist)
import Control.Monad.Combinators.Expr

type ExpressionBlock = [Expression]

data Expression where
  Assignment :: String -> Expression -> Expression
  Variable :: String -> Expression
  ValueExpression:: Value -> Expression
  FunctionExpression :: Function -> ExpressionBlock -> Expression
  BinaryOperation :: BinOp -> Expression -> Expression -> Expression
  Negation :: Expression -> Expression
  deriving Show

data Scalar
  = R Rational
  | F Float
  deriving (Show, Eq)

newtype Vector = Vector [Scalar]
  deriving (Show, Eq)

data Matrix = Matrix [Scalar] Int Int
  deriving (Show, Eq)

data Value
  = S Scalar
  | M Matrix
  | V Vector
  | VL [Vector]
  deriving (Show, Eq)

data Function
  = INVERSE
  | RREF
  | EF
  | SPAN
  | DETERMINANT
  | PROJECT
  | DIM
  | RANK
  | NULLITY
  | IS_CONSISTENT
  | COL
  | ROW
  | NUL
  | SPANS
  | IS_BASIS
  | QR
  | AUGMENT
  | TRANSPOSE
  | ORTHO_BASIS
  | IN_SPAN
  | IS_INDEPENDENT
  | EIGENSPACE
  | IS_EIGENVALUE
  | IS_EIGENVECTOR
  deriving (Show, Enum)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  deriving Show

type Parser = Parsec Void String

whitespace :: Parser ()
whitespace = L.space
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
pAssignment = Assignment <$> pVarName <* reserved "<-" <*> pExpression

pVarName :: Parser String
pVarName = undefined

pExpression :: Parser Expression
pExpression = makeExprParser pAtom operatorTable

pAtom :: Parser Expression
pAtom = choice
  [ pValueExpression
  , pFunction
  , pVariable ]

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [ prefix "-" Negation
    ]
  , [ binary "*" Mul
    , binary "/" Div
    ]
  , [ binary "+" Add
    , binary "-" Sub
    ]
  ]

binary :: String -> BinOp -> Operator Parser Expression
binary  name f = InfixL  (BinaryOperation f <$ symbol name)

prefix :: String -> (Expression -> Expression) -> Operator Parser Expression
prefix  name f = Prefix  (f <$ symbol name)

pVariable :: Parser Expression
pVariable = Variable <$> identifier

pFunction :: Parser Expression
pFunction = FunctionExpression
  <$> choice (map pFunctionName (enumFrom INVERSE))
  <*> pArgs

pFunctionName :: Function -> Parser Function
pFunctionName f = f <$ reserved (show f)

pArgs :: Parser ExpressionBlock
pArgs = parens (sepBy pExpression (symbol ","))

pValueExpression :: Parser Expression
pValueExpression = ValueExpression <$> pValue

pValue :: Parser Value
pValue = choice
  [ S <$> pScalar
  , V <$> pVector
  , M <$> pMatrix
  , VL <$> pVectorList ]

pVector :: Parser Vector
pVector = Vector <$> between (symbol "<") (symbol ">") pRow

pVectorList :: Parser [Vector]
pVectorList = between (symbol "<") (symbol ">") (sepBy1 pVector (symbol ","))

pMatrix :: Parser Matrix
pMatrix = constructMatrix =<< between (symbol "[") (symbol "]") (sepBy1 pRow (symbol "|"))

constructMatrix :: [[Scalar]] -> Parser Matrix
constructMatrix l = if all ((== (length $ head l)) . length) (tail l)
  then return $ Matrix (concat l) ((length . head) l) (length l)
  else fail "All rows in a matrix must be of equal length."

pRow :: Parser [Scalar]
pRow = some pScalar

pScalar :: Parser Scalar
pScalar = choice
  [ F <$> L.signed whitespace float
  , R . fromIntegral <$> L.signed whitespace integer]

pInput :: Parser ExpressionBlock
pInput = whitespace *> pExpressionBlock <* eof

parseInput :: String -> Either String ExpressionBlock
parseInput s = case parse pInput "" s of
  Left err -> Left $ errorBundlePretty err
  Right e -> Right e