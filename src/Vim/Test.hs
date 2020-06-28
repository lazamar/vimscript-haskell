module Vim.Test where

{-
   A module to allow testing Vimscript
-}

import Vim.Expression
    (Vim, gen, eval, Depth(..), statement, Arg(..), Statement(..), Expr, generateCode)
import Control.Exception (bracket)
import GHC.IO.Handle (hPutStr, hGetContents, hClose)
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import System.IO.Temp (emptySystemTempFile)
import System.Process (readProcessWithExitCode, callCommand)


-- | Execute a vim script and return what it
-- printed in a buffer
execute :: String -> IO (Either String String)
execute code =
    withTempFile "input" $ \inputFile ->
        withTempFile "output" $ \outputFile -> do
            writeFile inputFile code
            callCommand $ unwords
                    [ "vim"
                    , "--clean"                           -- Start without any plugins
                    , "-S", inputFile                     -- Read our script
                    , "-c", "':wq! " <> outputFile <> "'" -- Save to file after executed
                    ]
            Right <$> readFile outputFile

-- | Create a temp file and delete after its use
withTempFile :: String -> (String -> IO a) -> IO a
withTempFile name = bracket (emptySystemTempFile name) removeFile

getResult :: Vim (Expr a) -> IO (Either String String)
getResult program =
    let code = generateCode $ do
            res <- program
            -- Add a statement that prints the result in the last
            -- line of the current buffer
            statement $ Call "let a =" [A res]
            statement $ Call "put =a" []
    in
    fmap (last . lines) <$> execute code

