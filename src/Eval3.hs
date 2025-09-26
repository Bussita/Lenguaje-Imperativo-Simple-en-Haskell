module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado vacío
initState :: State
initState = (M.empty, "")

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v (sigma, _) = case M.lookup v sigma of
  Just x  -> Right x
  Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update v a (sigma, trace) = (M.insert v a sigma, trace)

-- Agrega una traza dada al estado
addTrace :: String -> State -> State
addTrace t (sigma, trace) = (sigma, trace ++ t ++ " ")

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip st = Right (Skip :!: st)
stepComm (Let v e) st = do
  (val :!: st') <- evalExp e st
  let st'' = update v val st'
      st''' = addTrace ("Let " ++ v ++ " " ++ show val) st''
  Right (Skip :!: st''')
stepComm (Seq Skip c2) st = Right (c2 :!: st)
stepComm (Seq c1 c2) st = do
  (c1' :!: st') <- stepComm c1 st
  Right (Seq c1' c2 :!: st')
stepComm (IfThenElse b c1 c2) st = do
  (cond :!: st') <- evalExp b st
  let tr = addTrace ("if " ++ show cond) st'
  if cond then Right (c1 :!: tr) else Right (c2 :!: tr)
stepComm (RepeatUntil c b) st =
  Right (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: st)

-- Evalúa una expresión
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const a) st = Right (a :!: st)
evalExp (Var v) st = do
  val <- lookfor v st
  Right (val :!: st)
evalExp (UMinus a) st = do
  (val :!: st') <- evalExp a st
  Right ((-val) :!: st')
evalExp (Plus a b) st = do
  (val1 :!: st1) <- evalExp a st
  (val2 :!: st2) <- evalExp b st1
  Right (val1 + val2 :!: st2)
evalExp (Minus a b) st = do
  (val1 :!: st1) <- evalExp a st
  (val2 :!: st2) <- evalExp b st1
  Right (val1 - val2 :!: st2)
evalExp (Times a b) st = do
  (val1 :!: st1) <- evalExp a st
  (val2 :!: st2) <- evalExp b st1
  Right (val1 * val2 :!: st2)
evalExp (Div a b) st = do
  (val1 :!: st1) <- evalExp a st
  (val2 :!: st2) <- evalExp b st1
  if val2 == 0
    then Left DivByZero
    else Right (val1 `div` val2 :!: st2)
evalExp (VarInc v) st = do
  val <- lookfor v st
  let val' = val + 1
      st' = update v val' st
      st'' = addTrace ("Varinc " ++ v) st'
  Right (val' :!: st'')

-- Booleanos
evalExp BTrue st = Right (True :!: st)
evalExp BFalse st = Right (False :!: st)
evalExp (Lt a b) st = do
  (val1 :!: st1) <- evalExp a st
  (val2 :!: st2) <- evalExp b st1
  Right (val1 < val2 :!: st2)
evalExp (Gt a b) st = do
  (val1 :!: st1) <- evalExp a st
  (val2 :!: st2) <- evalExp b st1
  Right (val1 > val2 :!: st2)
evalExp (Eq a b) st = do
  (val1 :!: st1) <- evalExp a st
  (val2 :!: st2) <- evalExp b st1
  Right (val1 == val2 :!: st2)
evalExp (NEq a b) st = do
  (val1 :!: st1) <- evalExp a st
  (val2 :!: st2) <- evalExp b st1
  Right (val1 /= val2 :!: st2)
evalExp (And a b) st = do
  (val1 :!: st1) <- evalExp a st
  (val2 :!: st2) <- evalExp b st1
  Right (val1 && val2 :!: st2)
evalExp (Or a b) st = do
  (val1 :!: st1) <- evalExp a st
  (val2 :!: st2) <- evalExp b st1
  Right ((val1 || val2) :!: st2)
evalExp (Not a) st = do
  (val :!: st') <- evalExp a st
  Right (not val :!: st')
