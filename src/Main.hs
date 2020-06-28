{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (Ord(..))
import Vim.Expression
import Vim.Test (execute)

main = do
    Right res <- execute $ generateCode $ v
    putStr res
    where
        generateCode :: Vim () -> String
        generateCode
          = unlines
          . concatMap (gen 0)
          . snd
          . eval (Depth 0)

        v :: Vim ()
        v = do
            sayHi <- define3 $ \number name1 name2 -> do
                return
                    $ cond (number >= 2)
                    (name1)
                    $ cond (number < 0)
                    (name2)
                    (str "Other")

            statement $ Call "let a =" [A $ sayHi 1 "Marcelo" "John"]
            statement $ Call "put =a" []
