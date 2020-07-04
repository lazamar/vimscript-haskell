{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Vim.Run where

{-
   A module to allow running Vimscript and getting the output.
-}

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Bifunctor (first)
import Data.List (intercalate)
import System.Exit (ExitCode(..))
import System.Process (readCreateProcessWithExitCode, shell)
import Vim.Expression (Vim, statement, Arg(..), Statement(..), Expr, generateCode, Code(..), Err(..))

data ExecError
    = VimExecutionFailure String
    | FailedCompiling  Err
    deriving (Show)

showExecError :: ExecError -> String
showExecError = \case
    VimExecutionFailure err -> unlines
        [ "VimExecutionFailure:"
        , indent err
        ]

    FailedCompiling err -> unlines
        [ "FailedCompiling:"
        , indent $ show err
        ]
    where
        indent = unlines . fmap (">>  " ++) . lines

-- | Run a Vimscript program and return the value of its output
getResult :: (MonadError ExecError m, MonadIO m) => Vim (Expr a) -> m String
getResult program = do
    code <- either throwError return $ getCode program
    res <- execute code
    -- Remove initial and final new lines
    return . intercalate "\n". tail . lines $ res

-- | Add a statement that prints only the outcome expression in a new buffer
getCode :: Vim (Expr a) -> Either ExecError Code
getCode program = first FailedCompiling $ generateCode $ do
        res <- program
        statement $ Call "execute \"normal ggdG\"" [] -- Clear buffer
        statement $ Call "let a =" [A res]            -- Assign return value to variable
        statement $ Call "put =a" []                  -- Write  return value to buffer
        statement $ Call "%%print" []                 -- Send buffer content to stdout

execute :: (MonadIO m, MonadError ExecError m) => Code -> m String
execute (Code code) = do
    (ex, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode (shell "vim --clean -esV") code
    case ex of
        ExitSuccess -> return stdout
        ExitFailure n -> throwError $ VimExecutionFailure stderr

