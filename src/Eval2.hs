module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty 


-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v sigma = case M.lookup v sigma of
  Just x  -> Right x
  Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update v a sigma = M.insert v a sigma

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip sigma = Right (Skip :!: sigma)
stepComm (Let v e) sigma = do
  (val :!: sigma') <- evalExp e sigma
  let sigma'' = update v val sigma'
  Right (Skip :!: sigma'')
stepComm (Seq Skip c2) sigma = Right (c2 :!: sigma)
stepComm (Seq c1 c2) sigma = do
  (c1' :!: sigma') <- stepComm c1 sigma
  Right (Seq c1' c2 :!: sigma')
stepComm (IfThenElse b c1 c2) sigma = do
  (cond :!: sigma') <- evalExp b sigma
  if cond then Right (c1 :!: sigma') else Right (c2 :!: sigma')
stepComm (RepeatUntil c b) sigma =
  Right (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: sigma)

-- Evalúa una expresión
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const a) sigma = Right (a :!: sigma)
evalExp (Var v) sigma = do
  val <- lookfor v sigma
  Right (val :!: sigma)
evalExp (UMinus a) sigma = do
  (val :!: sigma') <- evalExp a sigma
  Right ((-val) :!: sigma')
evalExp (Plus a b) sigma = do
  (val1 :!: sigma1) <- evalExp a sigma
  (val2 :!: sigma2) <- evalExp b sigma1
  Right (val1 + val2 :!: sigma2)
evalExp (Minus a b) sigma = do
  (val1 :!: sigma1) <- evalExp a sigma
  (val2 :!: sigma2) <- evalExp b sigma1
  Right (val1 - val2 :!: sigma2)
evalExp (Times a b) sigma = do
  (val1 :!: sigma1) <- evalExp a sigma
  (val2 :!: sigma2) <- evalExp b sigma1
  Right (val1 * val2 :!: sigma2)
evalExp (Div a b) sigma = do
  (val1 :!: sigma1) <- evalExp a sigma
  (val2 :!: sigma2) <- evalExp b sigma1
  if val2 == 0
    then Left DivByZero
    else Right (val1 `div` val2 :!: sigma2)
evalExp (VarInc v) sigma = do
  val <- lookfor v sigma
  let val' = val + 1
      sigma' = update v val' sigma
  Right (val' :!: sigma')

-- Booleanos
evalExp BTrue sigma = Right (True :!: sigma)
evalExp BFalse sigma = Right (False :!: sigma)
evalExp (Lt a b) sigma = do
  (val1 :!: sigma1) <- evalExp a sigma
  (val2 :!: sigma2) <- evalExp b sigma1
  Right (val1 < val2 :!: sigma2)
evalExp (Gt a b) sigma = do
  (val1 :!: sigma1) <- evalExp a sigma
  (val2 :!: sigma2) <- evalExp b sigma1
  Right (val1 > val2 :!: sigma2)
evalExp (Eq a b) sigma = do
  (val1 :!: sigma1) <- evalExp a sigma
  (val2 :!: sigma2) <- evalExp b sigma1
  Right (val1 == val2 :!: sigma2)
evalExp (NEq a b) sigma = do
  (val1 :!: sigma1) <- evalExp a sigma
  (val2 :!: sigma2) <- evalExp b sigma1
  Right (val1 /= val2 :!: sigma2)
evalExp (And a b) sigma = do
  (val1 :!: sigma1) <- evalExp a sigma
  (val2 :!: sigma2) <- evalExp b sigma1
  Right (val1 && val2 :!: sigma2)
evalExp (Or a b) sigma = do
  (val1 :!: sigma1) <- evalExp a sigma
  (val2 :!: sigma2) <- evalExp b sigma1
  Right ((val1 || val2) :!: sigma2)
evalExp (Not a) sigma = do
  (val :!: sigma') <- evalExp a sigma
  Right (not val :!: sigma')
