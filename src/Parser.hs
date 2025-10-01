module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST
import GHC.Num (Num(fromInteger))

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until","case"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "{"
                        , "}"
                        , "++"
                        , "("
                        , ")"
                        , ":"
                        ]
    }
  )


-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = addSubExp -- Por qué no parsea ( y = -x++ ) pero sí parsea y = (-x++)

-- + y - binarios (menor precedencia) por eso primero se aplica mulDivExp
addSubExp :: Parser (Exp Int)
addSubExp = chainl1 mulDivExp addSubOp

addSubOp :: Parser (Exp Int -> Exp Int -> Exp Int)
addSubOp = (do reservedOp lis "+"
               return Plus)
       <|> (do reservedOp lis "-"
               return Minus)

mulDivExp :: Parser (Exp Int)
mulDivExp = chainl1 unaryExp mulDivOp

mulDivOp :: Parser (Exp Int -> Exp Int -> Exp Int)
mulDivOp = (do reservedOp lis "*"
               return Times)
       <|> (do reservedOp lis "/"
               return Div)

unaryExp :: Parser (Exp Int)
unaryExp = (try pUMinus) <|> postfixExp

pUMinus :: Parser (Exp Int)
pUMinus = do
            reservedOp lis "-"
            e <- unaryExp
            return (UMinus e)

postfixExp :: Parser (Exp Int)
postfixExp = do
  e <- atomicIntExp
  postfixCheck e  <|> return e -- Básicamente parseamos una expresión atómica, después de eso tratamos de matchear un ++ seguido de una variable
-- si no matchea, entonces devolvemos nuestra expresión tal cual

postfixCheck :: Exp Int -> Parser (Exp Int)
postfixCheck e = do
                reservedOp lis "++"
                case e of
                    Var v -> return (VarInc v)
                    _ -> fail "++ solo se puede aplicar a variables"
-- Expresiones atómicas
atomicIntExp :: Parser (Exp Int)
atomicIntExp = (try pConst) <|> (try pVar) <|> (try (parens lis intexp)) 

pConst :: Parser (Exp Int)
pConst = do
  n <- natural lis
  return (Const (fromInteger n))

pVar :: Parser (Exp Int)
pVar = do
  s <- identifier lis
  return (Var s)
------------------------------------
--- Parser de expresiones booleanas
------------------------------------
boolexp :: Parser (Exp Bool)
boolexp = orExp

orExp :: Parser (Exp Bool)
orExp = chainl1 andExp orOp

orOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orOp = do
  reservedOp lis "||"
  return Or

andExp :: Parser (Exp Bool)
andExp = chainl1 notExp andOp

andOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
andOp = do
  reservedOp lis "&&"
  return And

notExp :: Parser (Exp Bool)
notExp = (try pNot) <|> pAtomicBool

pAtomicBool :: Parser (Exp Bool)
pAtomicBool = (try pBasicBool) <|> (try (pBinaryBool "==" Eq)) <|> (try (pBinaryBool "!=" NEq)) <|> (try (pBinaryBool "<" Lt)) <|> (try (pBinaryBool ">" Gt)) <|> (try(parens lis boolexp))

pBasicBool :: Parser (Exp Bool)
pBasicBool = (try pTrue) <|> pFalse

pTrue :: Parser (Exp Bool)
pTrue = do
          reserved lis "true"
          return BTrue

pFalse :: Parser (Exp Bool)
pFalse = do
          reserved lis "false"
          return BFalse

pBinaryBool :: String -> (Exp Int -> Exp Int -> Exp Bool) -> Parser (Exp Bool)
pBinaryBool s op = do
          e1 <- intexp
          reservedOp lis s
          e2 <- intexp
          return (op e1 e2)

pNot :: Parser (Exp Bool)
pNot = do
          reservedOp lis "!"
          e1 <- notExp
          return (Not e1)

pAnd :: Parser (Exp Bool)
pAnd = do
          b1 <- boolexp
          reservedOp lis "&&"
          b2 <- boolexp
          return (And b1 b2)

pOr :: Parser (Exp Bool)
pOr = do
          b1 <- boolexp
          reservedOp lis "||"
          b2 <- boolexp
          return (Or b1 b2)

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 simpleComm seqOp 

simpleComm :: Parser Comm
simpleComm = (try pSkip)  <|> (try pAssign) <|> (try pIf) <|> (try pRepeat) <|> (try pCase)

seqOp :: Parser (Comm -> Comm -> Comm)
seqOp = do
  reservedOp lis ";"
  return Seq


pSkip :: Parser Comm
pSkip = do
          reserved lis "skip"
          return Skip

pAssign :: Parser Comm
pAssign = do
            x <- identifier lis
            reservedOp lis "="
            exp <- intexp
            return (Let x exp)

pIf :: Parser Comm
pIf = do
        reserved lis "if"
        b <- boolexp
        reservedOp lis "{"
        c <- comm
        reservedOp lis "}"
        rest b c <|> return (IfThen b c)

rest :: (Exp Bool) -> Comm -> Parser Comm
rest b c= do
          reserved lis "else"
          reservedOp lis "{"
          c1 <- comm
          reservedOp lis "}"
          return (IfThenElse b c c1)

pRepeat :: Parser Comm
pRepeat = do
            reserved lis "repeat"
            reservedOp lis "{"
            c <- comm
            reservedOp lis "}"
            reserved lis "until"
            b <- boolexp
            return (RepeatUntil c b)

pCase :: Parser Comm
pCase = do
  reserved lis "case"
  reservedOp lis "{"
  clauses <- many pCaseClause
  reservedOp lis "}"
  return (buildCase clauses)

pCaseClause :: Parser (Exp Bool, Comm)
pCaseClause = do
  b <- boolexp
  reservedOp lis ":"
  reservedOp lis "{"
  c <- comm
  reservedOp lis "}"
  return (b, c)

buildCase :: [(Exp Bool, Comm)] -> Comm
buildCase [] = Skip
buildCase ((b, c) : rest) = IfThenElse b c (buildCase rest)

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
