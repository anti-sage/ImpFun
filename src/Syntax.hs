module Syntax where

import Data.IORef

data Ast =
    SSkip
  | SIf Expr Stmt Stmt
  | SWhile Expr Stmt
  | SBlock Stmt
  | SSeq Stmt Stmt
  | SAssign String Expr
  | SVarDecl String Expr
  | SExpr Expr
  | SReturn Expr
  | STry Stmt String Stmt
  | SThrow Expr

  | EVal Value 
  | EVar String 
  | EFun [String] Stmt 
  | ECall Expr [Expr] [Value] 
  | ERef Expr 
  | EDeref Expr
  | ESpawn Expr
  | EDetach Expr
  | EJoin Expr
    deriving Show

type Stmt = Ast
type Expr = Ast

data Value = 
    VInt Int
  | VBool Bool
  | VString String
  | VRef (IORef Value)
  | VVoid
  | VClosure [String] Stmt Env
  | VPrimFun ([Value] -> Value)
  | VPrimFunIO ([Value] -> IO Value)
  | VThread Value

isValue, notValue :: Ast -> Bool
isValue (EVal _) = True
isValue _ = False
notValue = not . isValue

expr2val :: Expr -> Value
expr2val (EVal v) = v

type Env = [(String, Value)]

instance Show Value where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VString s) = s
  show (VRef _) = "ref"
  show (VVoid) = "void"
  show (VClosure _ _ _) = "closure"
  show (VPrimFun _) = "prim-fun"
  show (VPrimFunIO _) = "prim-fun io"

data Ctx = 
    CtxIf Stmt Stmt
  | CtxBlock Env
  | CtxSSeq Stmt
  | CtxAssign String 
  | CtxVarDecl String
  | CtxSExpr
  | CtxReturn
  | CtxTry String Stmt
  | CtxThrow
  
  | CtxCall0 [Expr] [Value] 
  | CtxCall1 Expr [Expr] [Value]
  | CtxCall Env
  
  | CtxRef
  | CtxDeref
  
  | CtxSpawn
  deriving (Show)

data Thread =
    Thread (Ast, Env, [Ctx]) [Thread]