{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module ExpressionSpec (spec) where


import Vim.Expression
import Text.Read (readMaybe)
import Test.Hspec.Expectations (HasCallStack)
import Test.Hspec (Spec, describe, it)
import Prelude hiding (Eq, True, False)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (runExceptT)

import qualified Prelude as P
import qualified Test.Hspec as Hspec
import qualified Vim.Run as Vim

spec :: Spec
spec = do
    describe "Primary types" $ do
        it "number" $ shouldEqual 1 $ return (1:: Expr Int)
        it "string" $ shouldEqual "hi" $ return ("hi" :: Expr String)
        it "string with escaped characters" $ shouldEqual "hi" $ return ("\"hi\"" :: Expr String)
        it "boolean False" $ shouldEqual P.False $ return false
        it "boolean True" $ shouldEqual P.True $ return true

shouldEqual :: forall a. (Show a, P.Eq a, VimRead a, HasCallStack)
    => a -> Vim (Expr a) -> IO ()
shouldEqual val program = do
    eitherRes <- runExceptT $ Vim.getResult program
    let res = vimRead $ either (error . show) id eitherRes
    Right val `Hspec.shouldBe` res

class VimRead a where
    vimRead :: String -> Either String a

instance VimRead Int where
    vimRead str = maybe (Left str) Right $ readMaybe str

instance VimRead String where
    vimRead = Right

instance VimRead Bool where
    vimRead str = case vimRead str :: Either String Int of
        Right 0 -> Right P.False
        Right 1 -> Right P.True
        _       -> Left str
