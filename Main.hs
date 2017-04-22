#!/usr/bin/env stack
{- --package server-generic --package text --package bytestring --package turtle --package wreq  --pacakge lens --package aeson --package unordered-containers --package containers --package wai-extra -}
{-# LANGUAGE RecordWildCards,OverloadedStrings,ScopedTypeVariables,TupleSections #-}
module Main where
import qualified Network.Wreq
import qualified Data.Vector
import qualified Data.Set as Set
import qualified Data.Text
import qualified Data.ByteString.Lazy as LBS
import Data.Text(Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as Data.Text
import qualified Data.Text.Encoding as LData.Text
import qualified Data.Text.Lazy     as LData.Text
import Data.Set(Set(..))
import Data.Function ((&))
import Control.Lens((^.),_Right,view,over)
import Data.List(intersperse)
import Control.Exception(catch,SomeException,bracket,finally)
import Data.Aeson
import Data.Maybe(mapMaybe,fromJust)
import Control.Monad
import Data.Aeson.Types
import Data.Monoid(mappend,(<>))
import Data.Either(rights)
import qualified Control.Concurrent.QSem as Async
import qualified Control.Concurrent      as Async
import qualified Control.Concurrent.Async as Async
import qualified Turtle
import qualified System.Environment as IO
import qualified System.IO          as IO
import System.IO.Unsafe             as IO
import qualified Debug.Trace        as DT
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
  deriving (Show,Bounded,Enum)

instance Show ZCode where
  show z@(ZCode ac c qc) = concat (Data.Text.unpack <$> [ac,c])

data ZillowR = ZData Page ZCode -- A request for information from Zillows db

type ZillowResponse b = Either Error b
type Error = Text

-- emptyZCode :: Initialise a zillow code using area category and code
emptyZCode :: AreaCategory -> AreaCode -> ZCode
emptyZCode ac c = ZCode ac c (\ic -> "Zill/" <> ac <> c <> "_" <> (Data.Text.pack (drop 3 (show ic))))

c0 = emptyZCode "Z" "90210" -- all homes in the 90210 zip code
edgewater = emptyZCode "N" "00487" 
brickell = emptyZCode "N" "00332" 

zillow :: ZillowR -> IndicatorCode -> IO (ZillowResponse (QuandlCode,LBS.ByteString))
zillow (ZData page code) ic = ((Network.Wreq.get url) >>= (pure . Right . (qcode,) . (^. Network.Wreq.responseBody))) 
                              `Control.Exception.catch`  --catch exceptions.
                              (\(_::SomeException) -> return (Left ("Zillow Err: " <> qcode)))
  where
    qcode = quandlCode code ic
    url = "https://www.quandl.com/api/v3/datasets/" ++ Data.Text.unpack(qcode) ++ ".json?" ++ "page=" ++ (show page) ++ "&api_key=" ++ apiKey

-- this calls to a ruby gem called ascii_charts for table rendering.
graph tbl qcode = do 
  let (String name) : (String description) : rows : _ = let Object recs = maybe emptyObject id $ (
                                                                       (decode tbl :: Maybe Value) >>= \(Object kvs) -> 
                                                                         HM.lookup "dataset" kvs)
                                                        in mapMaybe (flip HM.lookup recs) ["name","description","data"]
  (Turtle.ExitSuccess, asciiGraph) <- 
    Turtle.shellStrict 
      ("bundle exec ruby ./graph.sh") 
      (return $ LData.Text.decodeUtf8 (LBS.toStrict (encode rows)))
  sequence_ $ (fmap Data.Text.putStrLn) [qcode,name,description,asciiGraph]

pg' = pg 0
pg n zcode = zillow (ZData n zcode)

apiKey = IO.unsafePerformIO (IO.getEnv "QUANDL_API_KEY") -- quandl supports github login: https://www.quandl.com/search?query=
ignoreErrors = \(_::SomeException) -> return () :: IO ()
quietly = flip catch ignoreErrors

throttle n tag = Async.newQSem n >>= pure . (,tag)

tick job p = Control.Exception.bracket 
             (Async.waitQSem (fst p))
             (const $ Async.signalQSem (fst p))
             (const job)
main = do
  let queries = [(minBound :: IndicatorCode) .. ] -- let's see what all the tables look like
      acts =        (zip (repeat $ pg' edgewater) queries) -- pull latest info on edgewater
              ++    (zip (repeat $ pg' brickell ) queries) -- pull latest info on brickell
  pipe     <- Async.newChan
  blocked  <- Async.newMVar [] :: IO (Async.MVar [Async.MVar ()])

  -- throttle handles so we can:
  --  - not hammer quandl
  --  - fetch network in parallel
  --  - draw to screen synchronously

  netT <- throttle 4 "Network.IO" 
  drawT <- throttle 1 "Draw.IO"

  let blkIO io = do
        m     <- Async.newEmptyMVar
        queue <- Async.takeMVar blocked
        Async.putMVar blocked (m:queue)
        Async.forkIO (quietly io `Control.Exception.finally` Async.putMVar m ())

  let printAnswers =
        Async.readChan pipe  >>= \info ->
        tick (uncurry (flip graph) info) drawT -- throttle drawing so we don't mash graphs

  let wait = do
        blk <- Async.takeMVar blocked
        case blk of
          []             -> return () -- done waiting
          (finaliser:fs) ->  Async.putMVar blocked fs *> 
                             Async.takeMVar finaliser *> 
                             wait

  let loop ((action,code):rest) = (blkIO (do
                                             x <- tick (action code) netT --throttle as quandl's API doesn't like being hammered
                                             case x of 
                                               Left _  -> Async.writeChan pipe ("","") -- :f MVars.. blocking .. etc.
                                               Right a -> Async.writeChan pipe a
                                             )) : loop rest
      loop [] =  []
  let p = blkIO (printAnswers)
  --sprinkle in printing otherwise all the network IO will be completed before a single graph is drawn.
  --see: https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Concurrent.html note on Pre-Emption
  sequenceA (intersperse p (loop acts)) `Control.Exception.finally` wait

