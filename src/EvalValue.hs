-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalValue :: Program -> Result 就行。
module EvalValue where

import AST
import Control.Monad.State

data Value
  = VBool Bool
  | VInt Int
  | VChar Char

  -- ... more
  deriving (Show, Eq, Ord)
data VarSave = Var String Value deriving (Show, Eq)
data Context = Context {
  getVar :: [VarSave]
                          } deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

getBool :: Expr -> ContextState Bool
getBool e = do
  ev <- eval e
  case ev of
    VBool b -> return b
    _ -> lift Nothing

getInt :: Expr -> ContextState Int
getInt e = do
  ev <- eval e
  case ev of
    VInt b -> return b
    _ -> lift Nothing

getVarValue ::String -> [VarSave] -> ContextState Value
getVarValue name [] = lift Nothing
getVarValue name ((Var varname v):xs) = do if varname==name then return v else getVarValue name xs

pushVar :: VarSave -> Context -> Context
pushVar x (Context xs) = Context (x:xs)

eval :: Expr -> ContextState Value
eval (EBoolLit b) = return $ VBool b

eval (EIntLit n) = return $ VInt n

eval (ECharLit c) = return $ VChar c

eval (ENot e) = getBool e >>= \b -> return (VBool $ not b)

eval (EAnd e1 e2) = do v1 <- getBool e1
                       if v1 then do v2 <- getBool e2
                                     return (VBool v2)
                             else return (VBool False)

eval (EOr e1 e2) = do v1 <- getBool e1
                      if v1 then return (VBool True)
                            else do v2 <- getBool e2
                                    return (VBool v2)

eval (EAdd e1 e2) = do v1 <- getInt e1
                       v2 <- getInt e2
                       return (VInt (v1+v2))

eval (ESub e1 e2) = do v1 <- getInt e1
                       v2 <- getInt e2
                       return (VInt (v1-v2))

eval (EMul e1 e2) = do v1 <- getInt e1
                       v2 <- getInt e2
                       return (VInt (v1*v2))

eval (EDiv e1 e2) = do v1 <- getInt e1
                       v2 <- getInt e2
                       if v2 == 0 then error "divided by zero" else return (VInt (div v1 v2))

eval (EMod e1 e2) = do v1 <- getInt e1
                       v2 <- getInt e2
                       if v2 == 0 then error "divided by zero" else return (VInt (mod v1 v2))

eval (EEq e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (VBool (v1==v2))

eval (ENeq e1 e2) = do v1 <- eval e1
                       v2 <- eval e2
                       return (VBool (v1/=v2))

eval (ELt e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (VBool (v1<v2))

eval (EGt e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (VBool (v1>v2))

eval (ELe e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (VBool (v1<=v2))

eval (EGe e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (VBool (v1>=v2))

eval (EIf e1 e2 e3) = do v1 <- getBool e1
                         if v1 then eval e2 else eval e3

eval (EVar name) = do (Context xs) <- get
                      v <- getVarValue name xs
                      put (Context xs)
                      return v

eval (EApply (ELambda (name, t) e1) e2) = do context <- get
                                             v <- eval e2
                                             put (pushVar (Var name v) context)
                                             res <- eval e1
                                             return res



-- ... more
eval _ = undefined

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context []


evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
