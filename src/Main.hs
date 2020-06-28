{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (Ord(..))
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
            sayHi <- define3 $ \number name1 name2 -> do
                let some = number * 5 :: Expr Int
                    other = some + number

                define3 $ \number name1 name2 -> do
                    return 2
                    return 5
                    return ("Hi" :: Expr String)

                return $ cond (other >= 2) name1 $ cond (number < 0) name2 (str "Other")

            return $ sayHi 1 "Marcelo" "John"


