{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module ExpressionSpec (spec) where


import Vim.Expression
    ( Expr
    , Vim
    , MonadVim(..)
    , false
    , str
    , true
    )
import Text.Read (readMaybe)
import Test.Hspec.Expectations (HasCallStack)
import Test.Hspec (Spec, describe, it, parallel)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (runExceptT)
import Test.QuickCheck (property, Arbitrary(..), suchThat, withMaxSuccess, choose, listOf)
import Data.Char (ord, chr)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Bifunctor (first)
import System.Timeout (timeout)

import qualified Test.Hspec as Hspec
import qualified Vim.Run as Vim
import qualified Vim.Expression as E

spec :: Spec
spec = parallel $ do
    describe "Primary types" $ do
        describe "Number" $ do
            it "can be generated from any Haskell number"
                $ withMaxSuccess 20
                $ property $ \number -> shouldEqual number $ return (fromIntegral number :: Expr Int)

        describe "String" $ do
            it "can be generated from any Haskell String"
                  $ property $ \(NonEmptyString string) ->
                      shouldEqual string $ return (fromString string :: Expr String)

        it "string" $ shouldEqual "hi" $ return ("hi" :: Expr String)
        it "string with escaped characters" $
            let testString =  "\\\"\"\''hi"
            in
            shouldEqual testString $ return $ str testString
        it "boolean False" $ shouldEqual False $ return false
        it "boolean True" $ shouldEqual True $ return true

shouldEqual :: forall a. (Show a, Eq a, VimRead a, HasCallStack)
    => a -> Vim (Expr a) -> IO ()
shouldEqual val program = do
    eitherRes <- withTimeout (100 * millisecond) $ runResult $ Vim.getResult program
    let res = vimRead $ either error id eitherRes
    res `Hspec.shouldBe` Right val
    where
        runResult = fmap (first Vim.showExecError) . runExceptT
        withTimeout t
            = fmap (fromMaybe (Left "Timeout"))
            . timeout (t * millisecond)
        millisecond = 1000

newtype NonEmptyString = NonEmptyString String
    deriving (Show)

instance Arbitrary NonEmptyString where
    arbitrary = fmap NonEmptyString validString
        where
            -- Only printable characters
            validChar = chr <$> choose (32, 126)
            validString = listOf validChar `suchThat` (not . null)


class VimRead a where
    vimRead :: String -> Either String a

instance VimRead Int where
    vimRead str = maybe (Left str) Right $ readMaybe str

instance VimRead String where
    vimRead = Right

instance VimRead Bool where
    vimRead str = case vimRead str :: Either String Int of
        Right 0 -> Right False
        Right 1 -> Right True
        _       -> Left str
