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
type QuandlCode   = Text
type Page         = Int

data ZCode   = ZCode {
  areaCategory:: AreaCategory
  ,areaCode :: AreaCode
  ,quandlCode :: IndicatorCode -> QuandlCode
  }

data IndicatorCode = IC_A   -- --- All Homes
  |IC_SF   -- Single Family Residences
  |IC_C   -- Condominiums
  |IC_MVSF   -- Price per Square Foot
  |IC_1B   -- 1 Bedroom
  |IC_2B   -- 2 Bedroom
  |IC_3B   -- 3 Bedroom
  |IC_4B   -- 4 Bedroom
  |IC_5B   -- 5 or More
  |IC_BT   -- Bottom Tier
  |IC_MT   -- Middle Tier
  |IC_TT   -- Top Tier
  |IC_RMP   -- Median Rent, Homes Listed for Rent
  |IC_RAH   -- Estimated Rent, All Homes in Region
  |IC_RZSF   -- Estimated Rent per Square Foot
  |IC_PRR   -- Price-to-Rent Ratio
  |IC_MLP   -- Median List Price
  |IC_MSP   -- Median Sale Price
  |IC_MLPSF   -- Median List Price per Square Foot
  |IC_MSPSF   -- Median Sale Price per Square Foot
  |IC_LPC   -- Listings with Price Cut in Last 30 Days
  |IC_MPC   -- Median Price Cut
  |IC_SLPR   -- Ratio of Sale Price to List Price
  |IC_SFL   -- Sold for Loss
  |IC_SFG   -- Sold for Gain
  |IC_IV   -- Increasing Values
  |IC_DV   -- Decreasing Values
  |IC_SPY   -- Turnover in Housing Market, Past 1 Year
  |IC_HR   -- Number of Homes for Rent
  |IC_HF   -- Monthly Foreclosures per 10,000 Homes
  |IC_FR   -- Percentage of Sales that were Foreclosures
  deriving Show

instance Show ZCode where
  show z@(ZCode ac c qc) = concat (Data.Text.unpack <$> [ac,c])

data ZillowR = ZData Page ZCode -- A request for information from Zillows db

-- emptyZCode :: Initialise a zillow code using area category and code
emptyZCode :: AreaCategory -> AreaCode -> ZCode
emptyZCode ac c = ZCode ac c (\ic -> "Zill/" <> ac <> c <> "_" <> (Data.Text.pack (drop 3 (show ic))))

c0 = emptyZCode "Z" "90210" -- all homes in the 90210 zip code
edgewater = emptyZCode "N" "00487" 

zillow (ZData page code) ic = do
  Data.Text.putStrLn( qcode )
  Network.Wreq.get url >>= return . (^. Network.Wreq.responseBody)
  where
    qcode = quandlCode code ic
    url = "https://www.quandl.com/api/v3/datasets/" ++ Data.Text.unpack(qcode) ++ ".json?" ++ "page=" ++ (show page) ++ "&api_key=" ++ apiKey

apiKey = IO.unsafePerformIO (IO.getEnv "QUANDL_API_KEY")
main = do
  zillow (ZData 0 c0) IC_SPY >>= LBS.putStrLn -- turnover for all homes in c0 "SPY"
  zillow (ZData 0 c0) IC_SFG >>= LBS.putStrLn -- sold for gains
  zillow (ZData 0 c0) IC_SFL >>= LBS.putStrLn -- sold for losses
  mconcat (zillow (ZData 0 edgewater) <$> [IC_SPY,IC_SFG,IC_SFL]) >>= LBS.putStrLn

