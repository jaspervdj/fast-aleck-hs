module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Text.FastAleck.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Text.FastAleck.Tests" Text.FastAleck.Tests.tests
    ]
