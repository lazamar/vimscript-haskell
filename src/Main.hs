{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude (putStrLn, unlines, concatMap, map, (.), ($), snd, return, Num(..))
import Vim.Expression

main = putStrLn . unlines . concatMap (gen 0) . snd . eval (Depth 0) $ v
    where
        v :: Vim ()
        v = do
            sayHi <- define3 $ \number name1 name2 -> do
                return $ cond
                    (number >= 2)
                    (name1)
                    (name2)

            echo $ sayHi (5 * 2) "Marcelo" "John"
