module ExpressionSpec (spec) where


import qualified Test.Hspec as Hspec
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations (HasCallStack)

spec :: Spec
spec = do
    describe "Tests" $ do
        it "work" $ 1 `Hspec.shouldBe` 1



