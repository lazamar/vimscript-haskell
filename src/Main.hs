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

                    f2 = lam $ \number ->
                         lam $ \name1  ->
                         lam $ \name2  ->
                            cond (number == 2) name1
                            $ cond (number > 3) name2
                            $ (str "Oi")

                return $ cond (other < 2) name1
                       $ cond (number < 0) name2
                       $ cond False "Other" $ f2 `app` number `app` name1 `app` str "No choice"

            return $ sayHi 4 "Marcelo" "John"


