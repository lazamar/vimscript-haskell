module Vim.Expression where

data Expr a
    = Const a
    | Var String
    | Fun
        { body   :: Expr a
        }

data Vim a = Vim (Expr a)
