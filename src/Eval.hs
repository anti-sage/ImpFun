module Eval where

import Syntax
import Primitive
import Pretty
import Debug.Trace (trace)

import Control.Monad
import Data.IORef

addVar :: String -> Value -> Env -> Env
addVar s v env = (s, v):env
  
addVars :: [String] -> [Value] -> Env -> Env
addVars ss vs env = zip ss vs ++ env

findVar :: String -> Env -> Value
findVar s env =
  let (Just v) = lookup s env in v -- assumes that a variable is always found

exec :: Ast -> IO ()
exec e = steps (e, primitives, [])

steps :: (Ast, Env, [Ctx]) -> IO ()
steps (SSkip, _, []) = return ()
steps st = step st >>= steps

stepsMT`:: Thread -> IO ()
stepsMT (Thread st ch) = do
  st' <- step st
  ch' <- runChildren ch
  return $ Thread st' ch'
    runChildren [] = []
    runChildren (x:xs) = stepsMT x >> runChildren xs

step :: (Ast, Env, [Ctx]) -> IO (Ast, Env, [Ctx])
-- step (ast, e, c) | trace ((show ast) ++ "\n--\n" ++ show c ++ "\n") False = undefined

-- Statement expresssion: evaluate expression and turn into SSkip
step (SExpr e, env, ctx) = return (e, env, CtxSExpr : ctx)
step (v, env, CtxSExpr : ctx) | isValue v = return (SSkip, env, ctx)

-- Blocks
step (SBlock s, env, ctx) = return (s, env, CtxBlock env : ctx)
step (SSkip, _, CtxBlock env : ctx) = return (SSkip, env, ctx)

-- Sequences
step (SSeq s1 s2, env, ctx) = return (s1, env, CtxSSeq s2 : ctx)
step (SSkip, env, CtxSSeq s2 : ctx) = return (s2, env, ctx)

-- If and while
step (SIf cond s1 s2, env, ctx) = return (cond, env, CtxIf s1 s2 : ctx)
step (EVal (VBool True), env, CtxIf s1 _ : ctx) = return (s1, env, ctx)
step (EVal (VBool False), env, CtxIf _ s2 : ctx) = return (s2, env, ctx)

step (w@(SWhile cond s), env, ctx) = return (SIf cond (SSeq s w) SSkip, env, ctx)

-- Variable declaration
step (SVarDecl s e, env, ctx) = return (e, env, CtxVarDecl s : ctx)
step (EVal v, env, CtxVarDecl s : ctx) = return (SSkip, addVar s v env, ctx)

-- Assignment
step (SAssign s e, env, ctx) = return (e, env, CtxAssign s : ctx)
step (v, env, CtxAssign s : ctx) | isValue v = do
  case findVar s env of
    (VRef nv) -> writeIORef nv (expr2val v) >> return (SSkip, env, ctx)
    _ -> ioError $ userError $ "Trying to assign to a non-ref \"" ++ s ++ "\""
  
-- Return
step (SReturn e, env, ctx) = return (e, env, CtxReturn : ctx)
step (v@(EVal _), _, CtxReturn : ctx) = return (v, env, ctx')
  where
    (CtxCall env : ctx') = unwind ctx
    unwind c@(CtxCall _ : ctx) = c
    unwind (_ : ctx) = unwind ctx

-- Try
step (STry ts s cs, env, ctx) = return (ts, env, CtxTry s cs : ctx)
step (SSkip, env, CtxTry _ _ : ctx) = return (SSkip, env, ctx)

-- Throw
step (SThrow e, env, ctx) = return (e, env, CtxThrow : ctx)
step ((EVal v), env, CtxThrow : ctx) = return (cs, (addVar s v env), ctx')
  where
    (CtxTry s cs : ctx') = unwind ctx
    unwind c@(CtxTry _ _ : ctx) = c
    unwind (_ : ctx) = unwind ctx

-- Spawn
step (ESpawn e, env, ctx) = return (e, env, CtxSpawn : ctx)
step (e@(EVal _), env, CtxSpawn : ctx) = return (e, env, ctx)

-- Detach
step (EDetach e, env, ctx) = return 

-- Join
step (EJoin e, env, ctx) = return ()

-- Variable reference: get from environment
step (EVar s, env, ctx) = return (EVal $ findVar s env, env, ctx)

-- Box a value
step (ERef e, env, ctx) = return (e, env, CtxRef : ctx)
step (EVal v, env, CtxRef : ctx) = do
  nv <- newIORef v  
  return (EVal (VRef nv), env, ctx)
                                
-- Dereference a ref
step (EDeref e, env, ctx) = return (e, env, CtxDeref : ctx)
step (EVal (VRef nv), env, CtxDeref : ctx) = do
  v <- readIORef nv
  return $ (EVal v, env, ctx)

-- Function becomes a closure
step (EFun pars body, env, ctx) = return (EVal $ VClosure pars body env, env, ctx)

-- Reduce on function position
step (ECall f args vs, env, ctx) | notValue f = return (f, env, CtxCall0 args vs : ctx)
step (v, env, CtxCall0 args vs : ctx) | isValue v = return (ECall v args vs, env, ctx)

-- Reduce on argument position
step (ECall f (a:args) vs, env, ctx) = return (a, env, CtxCall1 f args vs : ctx)
step ((EVal v), env, CtxCall1 f args vs : ctx) = return (ECall f args (v:vs), env, ctx)

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step (ECall (EVal (VClosure pars s cloEnv)) [] vs, env, ctx) = do
  return (s, addVars pars (reverse vs) cloEnv, CtxCall env : ctx)
step (SSkip, _, CtxCall env : ctx) = return (EVal VVoid, env, ctx)
step (ECall (EVal (VPrimFun f)) [] vs, env, ctx) = do
  return (EVal $ f (reverse vs), env, ctx)
step (ECall (EVal (VPrimFunIO f)) [] vs, env, ctx) = do
  res  <- f (reverse vs)
  return (EVal res, env, ctx)
