module Vim.Test where

import Control.Exception (bracket)
import GHC.IO.Handle (hPutStr, hGetContents, hClose)
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import System.IO.Temp (emptySystemTempFile)
import System.Process (readProcessWithExitCode, callCommand)

{-
   A module to allow testing the scripts
-}

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

withTempFile :: String -> (String -> IO a) -> IO a
withTempFile name = bracket (emptySystemTempFile name) removeFile

