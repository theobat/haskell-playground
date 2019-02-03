{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           Control.Concurrent
import           Control.Monad
import Data.Vector (Vector)
import qualified Data.Vector                   as Vect
import qualified Data.Aeson as JSON
import           Control.Parallel.Strategies
import           Data.Scientific               as Scientific
import           System.Environment
import Debug.Trace

someFunc :: Bool -> IO ()
someFunc isParallel = case isParallel of
    True -> print (parMapVec complexComputation randomData)
    False -> print (Vect.map complexComputation randomData)

complexComputation :: JSON.Value -> Int
complexComputation (JSON.Number a) =
    traceShow (a) . loosingSum $ fromIntegral (coefficient a)

isLoosing :: Int -> Int -> Bool
isLoosing x y
    | y < x = isLoosing y x
    | x == 0 = True
    | x == 1 = False
    | y `mod` x == 0 = False
    | otherwise = and
        [ not (isLoosing (y - x * m) x) | m <- [1 .. (y `div` x)] ]

loosingSum :: Int -> Int
loosingSum n = sum
    [ x + y | x <- [1 .. (n - 1)], y <- [(x + 1) .. n], isLoosing x y == True ]

-- dxs = parMap rpar f [1, 2, 3, 4]

-- main = print (dxs)s

parMapVec :: (a -> b) -> Vector a -> Vector b
parMapVec f v = runEval $ parEval (parTraversable rpar $ Vect.map f v)


randomData =
    case
            (JSON.decode
                "[40,41,42,43,44,45,46,47,48,49,33,34,35, 40,41,42,43,44,45,46,47,48,49,33,34,35]"
            ) :: Maybe JSON.Array
        of
    Just arr -> arr
    Nothing -> Vect.empty