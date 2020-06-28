module Vim.Run where

{-
   A module to allow running Vimscript and getting the output.
-}

import Vim.Expression (Vim, statement, Arg(..), Statement(..), Expr, generateCode)
import Control.Exception (bracket, try, IOException)
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import System.Process (callCommand)

data ExecError
    = FailedRunningVim IOException
    deriving (Show)

-- | Run a Vimscript program and return the value of its output
getResult :: Vim (Expr a) -> IO (Either ExecError String)
getResult program =
    let code = generateCode $ do
            res <- program
            -- Add a statement that prints the result in the last
            -- line of the current buffer
            statement $ Call "let a =" [A res]
            statement $ Call "put =a" []
    in
    fmap (last . lines) <$> execute code

-- | Execute a vim script and return what it
-- printed in a buffer
execute :: String -> IO (Either ExecError String)
execute code =
    withTempFile "input" $ \inputFile ->
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

-- | Create a temp file and delete after its use
withTempFile :: String -> (String -> IO a) -> IO a
withTempFile name = bracket (emptySystemTempFile name) removeFile
