-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import AST
import Control.Monad.State

data VarSave = Var String Type deriving (Show, Eq)
data Context = Context {
    getVar :: [VarSave]
                       }
  deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

isBool :: Expr -> ContextState Type
isBool e = do
  et <- eval e
  case et of
    TBool -> return TBool
    _ -> lift Nothing

isArrow :: Expr -> ContextState Type
isArrow e = do
  et <- eval e
  case et of
    TArrow t1 t2 -> return (TArrow t1 t2)
    _ -> lift Nothing

isInt :: Expr -> ContextState Type
isInt e = do
  et <- eval e
  case et of
    TInt -> return TInt
    _ -> lift Nothing

isChar :: Expr -> ContextState Type
isChar e = do
  et <- eval e
  case et of
    TChar -> return TChar
    _ -> lift Nothing

canbeEq :: Expr -> ContextState Type
canbeEq e = do et <- eval e
               case et of 
                  TBool -> return TBool
                  TInt -> return TInt
                  TChar -> return TChar
                  _ -> lift Nothing

canbeOrd :: Expr -> ContextState Type
canbeOrd e  = do et <- eval e
                 case et of 
                   TInt -> return TInt
                   TChar -> return TChar
                   _ -> lift Nothing

removeVar :: String -> Context -> Context
removeVar name (Context xs) = Context (removeloop name xs)

removeloop :: String -> [VarSave] -> [VarSave]
removeloop name [] = error "No var found"
removeloop name (x:xs) = if varname==name then xs else x:(removeloop name xs)
                  where (Var varname _) = x

getVarType ::String -> [VarSave] -> ContextState Type
getVarType name [] = lift Nothing
getVarType name ((Var varname t):xs) = do if varname==name then return t else getVarType name xs

pushVar :: VarSave -> Context -> Context
pushVar x (Context xs) = Context (x:xs)

eval :: Expr -> ContextState Type
eval (EBoolLit _) = return TBool
eval (EIntLit _) = return TInt
eval (ECharLit _) = return TChar
eval (ENot e) = isBool e >> return TBool
eval (EAnd e1 e2) = isBool e1 >> isBool e2 >> return TBool
eval (EOr e1 e2) = isBool e1 >> isBool e2 >> return TBool
eval (EAdd e1 e2) = isInt e1 >> isInt e2 >> return TInt
eval (ESub e1 e2) = isInt e1 >> isInt e2 >> return TInt
eval (EMul e1 e2) = isInt e1 >> isInt e2 >> return TInt
eval (EDiv e1 e2) = isInt e1 >> isInt e2 >> return TInt
eval (EMod e1 e2) = isInt e1 >> isInt e2 >> return TInt
eval (EEq e1 e2) = do type1 <- canbeEq e1
                      type2 <- canbeEq e2
                      if type1 == type2 then return TBool else lift Nothing
eval (ENeq e1 e2) = do type1 <- canbeEq e1
                       type2 <- canbeEq e2
                       if type1 == type2 then return TBool else lift Nothing
eval (ELt e1 e2) = do type1 <- canbeOrd e1
                      type2 <- canbeOrd e2
                      if type1 == type2 then return TBool else lift Nothing
eval (EGt e1 e2) = do type1 <- canbeOrd e1
                      type2 <- canbeOrd e2
                      if type1 == type2 then return TBool else lift Nothing
eval (ELe e1 e2) = do type1 <- canbeOrd e1
                      type2 <- canbeOrd e2
                      if type1 == type2 then return TBool else lift Nothing
eval (EGe e1 e2) = do type1 <- canbeOrd e1
                      type2 <- canbeOrd e2
                      if type1 == type2 then return TBool else lift Nothing
eval (EIf e1 e2 e3) = do v1 <- isBool e1
                         v2 <- eval e2
                         v3 <- eval e3
                         if v2==v3 then return v2 else lift Nothing
eval (EVar n) = do (Context xs) <- get
                   put $ Context xs
                   t <- getVarType n xs
                   return t
eval (ELambda (name, t) e) = do context <- get
                                put (pushVar (Var name t) context)
                                returntype <- eval e
                                put context
                                return (TArrow t returntype)
                   
eval (EApply func e) = do (TArrow t1 t2) <- isArrow func
                          t3 <- eval e
                          if t1==t3 then return t2 else lift Nothing


eval (ELet (name, e1) e2) = do t1 <- eval e1
                               context <- get
                               put (pushVar (Var name t1) context)
                               t2 <- eval e2
                               put context
                               return t2

eval (ELetRec name (arg, argType) (e1, returnType) e2) = do context <- get
                                                            put (pushVar (Var arg argType) context)
                                                            newcontext <- get
                                                            put (pushVar (Var name (TArrow argType returnType)) newcontext)
                                                            t1 <- eval e1
                                                            if t1 /= returnType then lift Nothing else do 
                                                                                                          t2 <- eval e2
                                                                                                          put context
                                                                                                          return t2



eval _ = undefined


evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $
  Context []-- 可以用某种方式定义上下文，用于记录变量绑定状态
