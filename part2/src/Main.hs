module Main (main) where

import System.Environment
import Args.Status

import Launch

main :: IO ()
main = do
    argv <- getArgs
    case isEmpty argv of
        True -> Launch.launchInterpreter
        False -> Launch.launchCompiler argv
