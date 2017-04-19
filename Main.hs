#!/usr/bin/env stack
{- --package server-generic --package text --package bytestring --package turtle --package wreq  --pacakge lens --package aeson --package unordered-containers --package containers --package wai-extra -}
{-# LANGUAGE RecordWildCards,OverloadedStrings #-}
module Main where
import qualified Network.Wreq
import qualified Data.Vector
import qualified Data.Set as Set
import qualified Data.Text
import qualified Data.ByteString.Lazy as LBS
import Data.Text(Text)
import qualified Data.Text.IO as Data.Text
import Data.Set(Set(..))
import Data.Function ((&))
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import qualified System.Environment as IO
import qualified System.IO          as IO
import System.IO.Unsafe             as IO

type AreaCategory = Text
type AreaCode     = Text
type IndicatorCode= Text
type QuandlCode   = Text

data ZCode   = ZCode {
  areaCategory:: AreaCategory
  ,areaCode :: AreaCode
  ,quandlCode :: IndicatorCode -> QuandlCode
  }

instance Show ZCode where
  show z@(ZCode ac c qc) = concat (Data.Text.unpack <$> [ac,c])

data ZillowR = ZData ZCode

apiKey = IO.unsafePerformIO (IO.getEnv "QUANDL_API_KEY")

emptyZCode ac c = ZCode ac c (\ic -> "Zill/" <> ac <> c <> "_" <> ic)

--turnover for all homes in the 90210 zip code
c0 = emptyZCode "Z" "90210" -- "SPY"

zillow (ZData code) = do
  Data.Text.putStrLn( qcode )
  Network.Wreq.get url >>= return . (^. Network.Wreq.responseBody)
    
  where
    qcode = quandlCode code "SPY"
    url = "https://www.quandl.com/api/v3/datasets/" ++ Data.Text.unpack(qcode) ++ ".json?api_key=" ++ apiKey

main = do
  zillow (ZData c0) >>= LBS.putStrLn

