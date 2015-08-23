{-|
Module      : Tiger.Tree
Description : This library defines the Tiger Tree IR
Copyright   : (c) Sherman Pay, 2015
License     : 
Maintainer  : shermanpay1991@gmail.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Tiger.Tree where

import qualified Tiger.Temporary as Temp

type Label = Temp.Label

data Expr = Const Int
              | Name Label
              | Temp Temp.Temp
              | Binop Binop Expr Expr
              | Mem Expr
              | Call Expr [Expr]
              | ESeq Stmt Expr

data Stmt = Move Expr Expr
              | Expr Expr
              | Jump Expr [Label]
              | CJump Relop Expr Expr Label Label
              | Seq Stmt Stmt
              | Label Label

data Binop =  Plus | Minus | Mul | Div | And | Or | Lshift | Rshift | Arshift | Xor

data Relop =  Eq | Ne | Lt | Gt | Le | Ge | Ult | Ule | Ugt | Uge 
