module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple             as T
import GHC.Base (VecElem(Int16ElemRep))

-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty 

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v sigma = sigma M.! v 

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v a sigma = M.insert v a sigma

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = T.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip sigma = Skip :!: sigma
stepComm (Let v e) sigma = let 
  r1 = evalExp e sigma 
  in Skip :!: (update v (T.fst r1) (T.snd r1))
stepComm (Seq Skip c2) sigma = c2 :!: sigma 
stepComm (Seq c1 c2) sigma = let 
  res = stepComm c1 sigma 
  in (Seq (T.fst res) c2) :!: T.snd res
stepComm (IfThenElse b c1 c2) sigma = let 
  res = evalExp b sigma 
  in case T.fst res of 
      True -> c1 :!: T.snd res
      False -> c2 :!: T.snd res
stepComm (RepeatUntil c b) sigma = (Seq c (IfThenElse b Skip (RepeatUntil c b))) :!: sigma

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const a) sigma = a :!: sigma
evalExp (Var a) sigma = lookfor a sigma :!: sigma 
evalExp (UMinus a) sigma = let r = evalExp a sigma in -T.fst r :!: T.snd r
evalExp (Plus a b) sigma = let 
  r1 = evalExp a sigma
  r2 = evalExp b (T.snd r1)
  in T.fst r1 + T.fst r2 :!: T.snd r2

evalExp (Minus a b) sigma = let
  r1 = evalExp a sigma
  r2 = evalExp b (T.snd r1)
  in T.fst r1 - T.fst r2 :!: T.snd r2

evalExp (Times a b) sigma = let
  r1 = evalExp a sigma
  r2 = evalExp b (T.snd r1)
  in T.fst r1 * T.fst r2 :!: T.snd r2

evalExp (Div a b) sigma = let
  r1 = evalExp a sigma
  r2 = evalExp b (T.snd r1)
  in T.fst r1 `div` T.fst r2 :!: T.snd r2

evalExp (VarInc v) sigma = let
  val = lookfor v sigma + 1
  sigma' = update v val sigma
  in val :!: sigma'

-- Booleanos
evalExp BTrue sigma = True :!: sigma
evalExp BFalse sigma = False :!: sigma

evalExp (Lt a b) sigma = let
  r1 = evalExp a sigma
  r2 = evalExp b (T.snd r1)
  in T.fst r1 < T.fst r2 :!: T.snd r2

evalExp (Gt a b) sigma = let
  r1 = evalExp a sigma
  r2 = evalExp b (T.snd r1)
  in T.fst r1 > T.fst r2 :!: T.snd r2

evalExp (Eq a b) sigma = let
  r1 = evalExp a sigma
  r2 = evalExp b (T.snd r1)
  in T.fst r1 == T.fst r2 :!: T.snd r2

evalExp (NEq a b) sigma = let
  r1 = evalExp a sigma
  r2 = evalExp b (T.snd r1)
  in T.fst r1 /= T.fst r2 :!: T.snd r2

evalExp (And a b) sigma = let
  r1 = evalExp a sigma
  r2 = evalExp b (T.snd r1)
  in T.fst r1 && T.fst r2 :!: T.snd r2

evalExp (Or a b) sigma = let
  r1 = evalExp a sigma
  r2 = evalExp b (T.snd r1)
  in (T.fst r1 || T.fst r2) :!: T.snd r2

evalExp (Not a) sigma = let
  r = evalExp a sigma
  in not (T.fst r) :!: T.snd r

