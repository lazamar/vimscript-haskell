module Main where

import Test.Hspec (hspec)

import qualified ExpressionSpec

main :: IO ()
main = hspec $ do
    ExpressionSpec.spec

