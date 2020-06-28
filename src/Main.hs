{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (Ord(..))
import Vim.Expression

import qualified Vim.Run as Vim

main = do
    res <- Vim.getResult $ do
        sayHi <- define3 $ \number name1 name2 -> do
            let n = number :: Expr Int
            return
                $ cond (n >= 2)
                (name1)
                $ cond (number < 0)
                (name2)
                (str "Other")

        return $ sayHi 1 "Marcelo" "John"
    putStrLn $ show res
