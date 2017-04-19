#!/usr/bin/env stack
{- --package server-generic --package text --package bytestring --package turtle --package wreq  --pacakge lens --package aeson --package unordered-containers --package containers --package wai-extra -}
{-# LANGUAGE RecordWildCards,OverloadedStrings #-}
module Main where
import qualified Network.Wreq
import qualified Data.Vector
import qualified Data.Set as Set
import qualified Data.Text
import qualified Data.Text.IO as Data.Text
import Data.Set(Set(..))
import Data.Function ((&))

main = Data.Text.putStrLn "Hello"

