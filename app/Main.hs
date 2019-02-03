{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    parallel <- pure (length args > 0 && head args == "yeah")
    _ <- print parallel
    someFunc parallel
        
