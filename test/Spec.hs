{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified AppSpec
import Test.Hspec (Spec, describe, hspec, it)

main :: IO ()
main = hspec AppSpec.spec
