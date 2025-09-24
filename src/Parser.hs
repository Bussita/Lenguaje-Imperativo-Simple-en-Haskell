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
                        ]
    }
  )


-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = (try pConst) <|> (try pVar) <|> pIntExp


pConst :: Parser (Exp Int)
pConst = do
          n <- natural lis
          return (Const (fromInteger n))

pVar :: Parser (Exp Int)
pVar = do
        s <- many1 letter
        return (Var s)

pIntExp :: Parser (Exp Int)
pIntExp = undefined
------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = (try pBasicBool) <|> (try pEqual) <|> (try pNotEqual) <|> (try pLess) <|> (try pGreater) <|> (try pNot) <|> (try pAnd) <|> (try pOr) <|> pBoolParen


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

pEqual :: Parser (Exp Bool)
pEqual = do
          e1 <- intexp
          reservedOp lis "=="
          e2 <- intexp
          return (Eq e1 e2)

pNotEqual :: Parser (Exp Bool)
pNotEqual = do
          e1 <- intexp
          reservedOp lis "!="
          e2 <- intexp
          return (NEq e1 e2)

pLess :: Parser (Exp Bool)
pLess = do
          e1 <- intexp
          reservedOp lis "<"
          e2 <- intexp
          return (Lt e1 e2)

pGreater :: Parser (Exp Bool)
pGreater = do
          e1 <- intexp
          reservedOp lis ">"
          e2 <- intexp
          return (Gt e1 e2)

pNot :: Parser (Exp Bool)
pNot = do
          reservedOp lis "!"
          e1 <- boolexp
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

pBoolParen :: Parser (Exp Bool)
pBoolParen = do
              reservedOp lis "("
              b <- boolexp
              reservedOp lis ")"
              return b

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 simpleComm seqOp 

simpleComm = (try pSkip)  <|> (try pAssign) <|> (try pIf) <|> (try pRepeat)

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
------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
