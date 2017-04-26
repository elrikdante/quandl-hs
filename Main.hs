#!/usr/bin/env stack
{- --package-mtl-2.2.1 --package text --package bytestring --package turtle --package wreq  --pacakge lens --package aeson --package unordered-containers --package containers -}
{-# LANGUAGE RankNTypes,FlexibleInstances,OverloadedStrings,ScopedTypeVariables,TupleSections, FlexibleContexts,MultiParamTypeClasses,RecordWildCards #-}
module Main where
import qualified Network.Wreq
import Data.Vector (Vector)
import qualified Data.Vector 
import Data.Text(Text)
import qualified Data.Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as Data.Text
import qualified Data.Text.Encoding as LData.Text
import qualified Data.Text.Lazy     as LData.Text
import Data.Function ((&))
import Control.Lens((^.),_Right,view,over)
import Control.Exception(catch,SomeException,bracket_,finally,handle)
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
import Turtle((<=<),(<|>))
import qualified System.Environment as IO
import qualified System.IO          as IO
import System.IO.Unsafe             as IO
import qualified Debug.Trace        as DT

import Control.Monad.Writer.Lazy as MTL

type AreaCategory = Text
type AreaCode     = Text
type QuandlCode   = Text
type Page         = Int
type Log a         = [a]

type ZillowM w b = WriterT (ThreadState IO_T w) IO b

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
type ZillowChunk = (QuandlCode,LBS.ByteString)
type Error = Text

-- emptyZCode :: Initialise a zillow code using area category and code
emptyZCode :: AreaCategory -> AreaCode -> ZCode
emptyZCode ac c = ZCode ac c (\ic -> "Zill/" <> ac <> c <> "_" <> (Data.Text.pack (drop 3 (show ic))))

c0 = emptyZCode "Z" "90210" -- all homes in the 90210 zip code
expositionCA= emptyZCode "Z" "90037"
edgewaterFL   = emptyZCode "N" "00487" 
brickellFL    = emptyZCode "N" "00332" 
losAngelesCA  = emptyZCode "N" "00014" 



zillow :: ZillowR -> IndicatorCode -> IO (ZillowResponse ZillowChunk)
zillow (ZData page code) ic = ((Network.Wreq.get url) >>= (pure . Right . (qcode,) . (^. Network.Wreq.responseBody))) 
                              `Control.Exception.catch`  --catch exceptions.
                              (\(_::SomeException) -> return (DT.trace (Data.Text.unpack $ "Zillow Err: " <> qcode) $ Left ("Zillow Err: " <> qcode)))
  where
    qcode = quandlCode code ic
    url = "https://www.quandl.com/api/v3/datasets/" ++ Data.Text.unpack(qcode) ++ ".json?" ++ "page=" ++ (show page) ++ "&api_key=" ++ apiKey

-- this calls to a ruby gem called ascii_charts for table rendering.
graph tbl qcode = do 
  let (String name) : (String description) : rows : _ = let Object recs = fromJust $ 
                                                                    (decode tbl >>= \(Object kvs) -> HM.lookup "dataset" kvs) 
                                                                <|> pure (object ["name" .= String qcode
                                                                                ,"description" .= String (qcode <> ": This graph failed to load")
                                                                                ,"data" .= Null])
                                                        in mapMaybe (flip HM.lookup recs) ["name","description","data"]
      _ = rows :: Value
  (Turtle.ExitSuccess, asciiGraph) <- 
    Turtle.shellStrict 
      ("bundle exec ruby -r ascii_charts -r json -e 'data=JSON(STDIN.read.chomp) rescue [] and data.any? && STDOUT.puts(AsciiCharts::Cartesian.new(data.first(18), bar: true).draw)'") 
      -- this runs much faster if bundle exec is removed from here,
      -- but ascii_charts will need to be installed on system gems as well.
      -- gem install ascii_charts
      (return $ LData.Text.decodeUtf8 (LBS.toStrict (encode rows)))
  sequence_ $ (fmap Data.Text.putStrLn) [qcode,name,description,asciiGraph]

pg' = pg 0
pg n zcode = ZData n zcode

apiKey       = IO.unsafePerformIO (IO.getEnv "QUANDL_API_KEY") -- quandl supports github login: https://www.quandl.com/search?query=
ignoreErrors = Control.Exception.handle (\(_::SomeException) -> return ())
quietly      = ignoreErrors

type ThrottleHandle rsc = (Async.QSem,rsc)

-- create n resource handles.
-- the program runs concurrently but the concurrency can be limited
-- 1 -> sequential execution
-- n > 1 -> concurrent execution limited to at most n
throttle :: Int -> IO_T -> IO (ThrottleHandle IO_T)
throttle n tag
  | n < 1     = error "throttle. : allocating 0 is fun if you like playing with blocked threads =)"
  | otherwise = Async.newQSem n >>= pure . (,tag)

data ThreadState rsc logTyp = ST' {
  tsinitialised :: Bool
  ,tsdone :: Bool
  ,tsHandles :: [ThrottleHandle rsc]
  ,tsLogs :: [logTyp]
  ,tsJobs :: [IO (ZillowResponse ZillowChunk)]
  ,tsResults :: [ZillowResponse ZillowChunk]
   }

instance Monoid (ThreadState r o) where
  mempty = ST' False False [] [] [] []
  (ST' li ld lh ll ljs lr) `mappend` (ST' ri rd rh rl rjs rr) = ST' (li || ri) (ld && rd) (lh++rh) (ll++rl) (ljs ++ rjs) (lr++rr)

addLog :: w -> ZillowM w ()
addLog w = pass $ pure ((),(\st@(ST'{..}) -> st { tsLogs= w:tsLogs }))

modify :: (ThreadState IO_T LogEntry -> ThreadState IO_T LogEntry) -> ZillowM LogEntry ()
modify f = pass $ pure ((),f)

gets :: ZillowM w (ThreadState IO_T w)
gets = do
  ((),st) <- listen (pure ())
  tell st
  return st

start,done :: ZillowM LogEntry ()
start = do
  info "Starting ZillowM"
  modify started
  where started st@(ST'{..}) = (st {tsinitialised = True})

initThrottleHandles = do
  info "Initialising Throttle Handles"
  ths <- liftIO (mapM (uncurry throttle) [(1,DrawIO),(2,NetworkIO),(7,ForkIO)])
  info "Initialised Throttle Handles"
  return ths

done = do
  info "All jobs completed. ZillowM Cleaning up"
  info "Finished ZillowM"
  modify fin
    where fin st@(ST'{..}) = (st {tsdone = True})

info,warn,err :: Text -> ZillowM LogEntry ()
info = addLog . Info
warn = addLog . Warning
err  = addLog . Main.Error

fork :: [ThrottleHandle IO_T] -> ZillowM LogEntry (ZillowResponse ZillowChunk) -> ZillowM LogEntry (ZillowResponse ZillowChunk)
fork ths action= do
  warn "Forking"
--  let drawT : netT : forkT : [] = ths
  res <- action
  warn "Done Forking"
  liftIO (uncurry (flip graph) . (^. _Right) $ res) *> return res

-- block until a handle is available, process the job and release the handle
-- this is guaranteed not to leak resources.
-- without this type sig we get skolem type variable errs. :s
tick :: ThrottleHandle IO_T -> ZillowM LogEntry a -> ZillowM LogEntry a
tick p job = do

  when (rsc == ForkIO) (info "Acquiring Fork Lock")
  when (rsc == NetworkIO) (info "Acquiring Net Lock")
  when (rsc == DrawIO) (info "Acquiring Draw Lock")

  (r,st') <- liftIO $ Control.Exception.bracket_
             (Async.waitQSem (fst p))
             (Async.signalQSem (fst p))
             (runWriterT job)
  tell st'
  when (rsc == ForkIO) (info "Releasing Fork Lock")
  when (rsc == NetworkIO) (info "Releasing Net Lock")
  when (rsc == DrawIO) (info "Releasing Draw Lock")

  return r
  where
    (_, rsc) = p

addRsc :: ThrottleHandle IO_T -> ZillowM LogEntry ()
addRsc h = do
  warn "Registering Throttle Handle"
  modify (\st@(ST'{..}) -> st { tsHandles= h:tsHandles })

addJob :: ZillowR -> IndicatorCode -> ZillowM LogEntry ()
addJob rq ic = do
  info ("Scheduling: " <> Data.Text.pack(show ic))
  modify (\st@(ST'{..}) -> st {tsJobs= zillow rq ic:tsJobs})

runJobs ioQ ths ((x,y):js) = do
  info ("Fetching: IC:" <> (Data.Text.pack (show y)))
  res <- fork ths (liftIO (zillow x y))
  case res of
    Left _ -> warn $ "Failed to fetch" <> (Data.Text.pack (show y))
    _      -> info ("Done Fetching: IC:" <> (Data.Text.pack (show y)))
  modify (\st@(ST'{..}) -> st {tsResults= res:tsResults})
  runJobs ioQ ths js


runJobs _ _ [] = return ()

runZillowM :: [(ZillowR,IndicatorCode)] -> ZillowM LogEntry [ZillowResponse ZillowChunk]
runZillowM jobs = do
  when (null jobs) (error "runZillowM: Nothing to do")
  ths <- initThrottleHandles
  info "Intialising Finalisers"
--  finalisers  <- liftIO $ Async.newMVar [] :: ZillowM LogEntry (Async.MVar [Async.MVar ()])
  info "Running Jobs"
  ((),ST'{..}) <- listen (sequence_ (uncurry addJob <$> jobs) *> start *> runJobs undefined ths jobs *> done)
  return tsResults

queries = [(minBound :: IndicatorCode) .. ] -- let's see what all the tables look like
testProgram = (zip (repeat $ pg' edgewaterFL)   queries) -- pull latest info on edgewater
               ++ (zip (repeat $ pg' brickellFL )   queries) -- pull latest info on brickell
               ++ (zip (repeat $ pg' losAngelesCA ) queries) -- pull latest info on LA
               ++ (zip (repeat $ pg' expositionCA ) queries) -- pull latest info on Exposition,LA
               ++ (zip (repeat $ pg' c0 ) queries)

data LogEntry = Warning Text
              | Info Text
              | Error Text deriving Show

data IO_T = NetworkIO | ForkIO | DrawIO deriving (Eq,Ord,Enum)

test = runWriterT $ runZillowM testProgram
main = do
  (_,ST'{..}) <- test
  mapM print tsLogs
