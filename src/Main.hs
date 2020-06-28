module Main where

import Vim.Expression

main = putStrLn . unlines . concatMap (gen 0) . snd . eval (Depth 0) $ v
    where
        v :: Vim ()
        v = do
            sayHi <- define1 $ \name -> do
                echo name
                return name
            call $ sayHi $ str "Marcelo"
