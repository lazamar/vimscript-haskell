{-# LANGUAGE FlexibleContexts #-}
module Vim.Run where

{-
   A module to allow running Vimscript and getting the output.
-}

import Control.Exception (bracket, try, IOException)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Bifunctor (first)
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import System.Process (callCommand)
import Vim.Expression (Vim, statement, Arg(..), Statement(..), Expr, generateCode, Code(..), Err(..))

data ExecError
    = FailedRunningVim IOException
    | FailedCompiling  Err
    deriving (Show)

-- | Run a Vimscript program and return the value of its output
getResult :: (MonadError ExecError m, MonadIO m) => Vim (Expr a) -> m String
getResult program = do
    code <- either throwError return $ getCode program
    res <- execute code
    return $ last $ lines res

-- | Add a statement that prints the result in the current buffer
getCode :: Vim (Expr a) -> Either ExecError Code
getCode program = first FailedCompiling $ generateCode $ do
        res <- program
        statement $ Call "let a =" [A res]
        statement $ Call "put =a" []

-- | Execute a vim script and return what it
-- printed in a buffer
execute :: (MonadIO m, MonadError ExecError m) => Code -> m String
execute (Code code) = do
    res <- liftIO $ withTempFile "input" $ \inputFile ->
        withTempFile "output" $ \outputFile -> do
            writeFile inputFile code
            res <- try $ callCommand $ unwords
                    [ "vim"
                    , "--clean"                           -- Start without any plugins
                    , "-S", inputFile                     -- Read our script
                    , "-c", "':wq! " <> outputFile <> "'" -- Save to file after executed
                    ]
            case res of
                Left err -> return $ Left $ FailedRunningVim err
                Right _ -> Right <$> readFile outputFile
    either throwError return res

-- | Create a temp file and delete after its use
withTempFile :: String -> (String -> IO a) -> IO a
withTempFile name = bracket (emptySystemTempFile name) removeFile
