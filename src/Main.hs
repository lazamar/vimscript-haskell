{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (Ord(..), True, False, Eq(..))
import Vim.Expression
import Control.Monad.Except

import qualified Vim.Run as Vim

main = do
    putStrLn "--  CODE  --"
    putStrLn $ unCode $ takeRight $ Vim.getCode program
    putStrLn "-- RESULT --"
    res <-  fmap takeRight $ runExceptT $ Vim.getResult program
    putStrLn $ show res
    where
        takeRight = either (error . show) id
        program = do
            sayHi <- define $ \number name1 name2 -> do
                let some = number * 5 :: Expr Int
                    other = some + number

                g <- define $ \number name1 name2 -> do
                    return $ cond (number == (1 :: Expr Int)) name1 name2

                return $ cond (other < 2) name1
                       $ cond (number < 0) name2
                       $ cond True (str "Other")
                       $ g 2 "hi" "hey"

            return $ sayHi 1 "Marcelo" "John"


