#!/usr/bin/env stack
{- --resolver lts-6.2 --package-mtl-2.2.1 --package text --package bytestring --package turtle --package wreq  --pacakge lens --package aeson --package unordered-containers --package containers -}
-- Copyright 2017 - Present Dante Elrik
{-# LANGUAGE RankNTypes,FlexibleInstances,OverloadedStrings,ScopedTypeVariables,TupleSections, FlexibleContexts,MultiParamTypeClasses,RecordWildCards #-}
module Main where
import qualified Network.Wreq
import Data.Vector (Vector)
import qualified Data.Vector 
import Data.Text(Text)
import qualified Data.Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text.IO       as Data.Text
import qualified Data.Text.Encoding as LData.Text
import qualified Data.Text.Lazy     as LData.Text
import Data.Function ((&))
import Control.Lens((^.),_Right,view,over)
import Control.Applicative(liftA3,liftA2)
import Control.Exception(catch,SomeException,bracket_,finally,handle)
import Data.Aeson
import Data.List(sortOn)
import Data.Maybe(mapMaybe,fromJust)
import Control.Monad
import Data.Aeson.Types
import Data.Monoid(mappend,(<>))
import Data.Either(rights)
import qualified Control.Concurrent.QSem  as Async
import qualified Control.Concurrent       as Async
import qualified Control.Concurrent.Async as Async
import qualified Turtle
import Turtle((<=<),(<|>))
import qualified System.Environment as IO
import qualified System.IO          as IO
import System.IO.Unsafe             as IO
import qualified Data.Time.Clock    as IO
import qualified Data.Time.Format   as IO
import qualified Debug.Trace        as DT
import Control.Monad.Writer.Lazy

type AreaCategory = Text
type AreaCode     = Text
type QuandlCode   = Text
type Page         = Int
type Log          = [LogEntry]

-- The ZillowM process has a Write-Only `ThreadState` and delivers a `b`
type ZillowM b = WriterT ThreadState IO b

data ZillowR = ZData Page ZCode -- A request for information from Zillows db

type ZillowResponse b = Either Error b
type ZillowChunk = (QuandlCode,LBS.ByteString)
type Error = Text

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
                              (\(_::SomeException) -> return (Left ("Zillow Err: " <> qcode)))
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
  l0 <- quickLog "Start Graph.Draw"
  (Turtle.ExitSuccess, asciiGraph) <- 
    Turtle.shellStrict 
      ("bundle exec ruby -r ascii_charts -r json -e 'data=JSON(STDIN.read.chomp) rescue [] and data.any? && STDOUT.puts(AsciiCharts::Cartesian.new(data.first(18), bar: true).draw)'") 
      -- this runs much faster if bundle exec is removed from here,
      -- but ascii_charts will need to be installed on system gems as well.
      -- gem install ascii_charts
      (return $ LData.Text.decodeUtf8 (LBS.toStrict (encode rows)))
  (sequence_ $ (fmap Data.Text.putStrLn) [qcode,name,description,asciiGraph])
  l1 <- quickLog "End Graph.Draw"
  return ((l0:). (l1:))

pg' = pg 0
pg n zcode = ZData n zcode

apiKey       = IO.unsafePerformIO (IO.getEnv "QUANDL_API_KEY") -- quandl supports github login: https://www.quandl.com/search?query=
ignoreErrors = Control.Exception.handle (\(_::SomeException) -> return ())
quietly      = ignoreErrors

quickLog = liftA2 (,) IO.getCurrentTime . pure . IOLog
{-# INLINE quickLog #-}

type ThrottleHandle rsc = (Async.QSem,rsc)

-- create n resource handles.
-- the program runs concurrently but the concurrency can be limited
-- 1 -> sequential execution
-- n > 1 -> concurrent execution limited to at most n
throttle :: Int -> IO_T -> IO (ThrottleHandle IO_T)
throttle n tag
  | n < 1     = error "throttle. : allocating 0 is fun if you like playing with blocked threads =)"
  | otherwise = Async.newQSem n >>= pure . (,tag)

-- computations can modify but not read `ThreadState`.
-- push and toggle based state manipulations
data ThreadState = ST' {
  tsinitialised :: Bool
  -- ^Whether or not we've initialised this thread via runZillowM
  ,tsdone       :: Bool
  -- ^Whether or not we've processed all jobs
  ,tsHandles    :: [ThrottleHandle IO_T]
  -- ^Handles for managing the different kinds of IO during job processing
  ,tsLogs       :: Log
  -- ^Ledger of all events
  ,tsJobs       :: [IO (ZillowResponse ZillowChunk)]
  -- ^Work Queue
  ,tsResults    :: [ZillowResponse ZillowChunk]
   -- ^Results of Work Queue
   }

instance Monoid (ThreadState) where
  mempty = ST' False False [] [] [] []
  {-# INLINE mempty #-}
  (ST' li ld lh ll ljs lr) `mappend` (ST' ri rd rh rl rjs rr) = ST' (li || ri) (ld && rd) (lh++rh) (ll++rl) (ljs ++ rjs) (lr++rr)
  {-# INLINE mappend #-}


addLog :: LogEntry -> ZillowM ()
addLog = flip addLogWithPrefix IO.getCurrentTime
{-# INLINE addLog #-}

addLogWithPrefix :: LogEntry -> IO IO.UTCTime ->  ZillowM ()
addLogWithPrefix w pf = liftIO (pf                         >>=
           pure 
           . Data.Text.pack 
           . IO.formatTime IO.defaultTimeLocale "[%s%Q] ") >>= \prefix 
           -> modify (\st@ST'{..} -> st { tsLogs=toText (prefix<>) w : tsLogs })
{-# INLINE addLogWithPrefix #-}

modify :: (ThreadState  -> ThreadState) -> ZillowM ()
modify f = pass $! pure ((),  f)
{-# INLINE modify #-}

start,done :: ZillowM ()
start = do
  info "Starting ZillowM"
  modify started
  where started st@(ST'{..}) = (st {tsinitialised = True})
{-# INLINE start #-}

initThrottleHandles = do
  -- throttle drawing so we don't mash graphs
  -- throttle forking because we don't want to spike memory
  -- throttle network too as quandl's API doesn't like being hammered
  info "Initialising Throttle Handles"
  ths <- liftIO (mapM (uncurry throttle) [(1,DrawIO),(2,NetworkIO),(8,ForkIO)])
  info "Initialised Throttle Handles"
  return ths

done = 
  info "All jobs completed. ZillowM Cleaning up" *>
  modify fin                                     *>
  info "Finished ZillowM"           
    where fin st@(ST'{..}) = (st {tsdone = True})
{-# INLINE done #-}

info,iolog,err :: Text -> ZillowM ()
info = addLog . Info
{-# INLINE info #-}
iolog = addLog . IOLog
{-# INLINE iolog #-}
err  = addLog . Main.Error
{-# INLINE err #-}

-- block until a handle is available, process the job and release the handle
-- this is guaranteed not to leak resources.
tick :: ThrottleHandle IO_T -> IO a -> IO a
tick p job =Control.Exception.bracket_
            (Async.waitQSem (fst p))
            (Async.signalQSem (fst p))
            job
{-# INLINE tick #-}

addRsc :: ThrottleHandle IO_T -> ZillowM ()
addRsc h =
  iolog "Registering Throttle Handle"                    *>
  modify (\st@(ST'{..}) -> st { tsHandles= h:tsHandles })

addJob :: ZillowR -> IndicatorCode -> ZillowM ()
addJob rq ic =
  info ("Scheduling: " <> Data.Text.pack(show ic))           *>
  modify (\st@(ST'{..}) -> st {tsJobs= zillow rq ic:tsJobs})
{-# INLINE addJob #-}

addResult :: ZillowResponse ZillowChunk -> ZillowM ()
addResult c =
  info ("Recording: " <> tag c)                          *>
  modify (\st@(ST'{..}) -> st {tsResults= c:tsResults})
  where
    tag (Left e)         = "Failure: " <> e
    tag (Right (zc,~_))  = "Success: " <> zc
{-# INLINE addResult #-}

-- run a set of queries againsts Zillow's Quandl Database.
-- each query will be printed as an ascii_graph
-- some queries may fail, there is no retry.

runZillowM :: [(ZillowR,IndicatorCode)] -> IO ([ZillowResponse ZillowChunk],ThreadState)
runZillowM jobs = runWriterT $ do
  when (null jobs) (error "runZillowM: Nothing to do")
  drawT : netT : forkT : [] <- initThrottleHandles
  liftIO $ IO.hSetBuffering IO.stdout IO.NoBuffering
  info "Intialising Finalisers"
  finalisers   <- liftIO $ Async.newMVar [] :: ZillowM (Async.MVar [Async.MVar IO_T])
  chan <- liftIO $ Async.newChan
  info "Scheduling Jobs"
  ((),ST'{..}) <- listen (sequence_ (uncurry addJob <$> jobs) *> start)
  info "Processing Started"
  jobsQueued   <- liftIO $ do
    flip mapM_ tsJobs (\job -> blkIO finalisers forkT forkT (tick netT job >>= Async.writeChan chan))
    length <$> Async.readMVar finalisers
  info ("Queued: " <> Data.Text.pack (show jobsQueued))
  info ("Beginning graph rendering")
  info ("Waiting on Asyncs to terminate")
  ((ioLogG,zillowChunks),ioLogF) <- liftA2 (,)
                                    (liftIO $ drawGraphs jobsQueued forkT drawT chan id id) 
                                    (liftIO $ waitIO finalisers id 0)
  ((),ST'{..})    <- listen (
    sequence_ (fmap addResult zillowChunks) >>= \_ -> 
      sequence_ $ (\(t,l) -> addLogWithPrefix l (pure t)) <$> (Data.List.sortOn fst (ioLogF++ioLogG))
    )
  info ("Backfilled IO Logs")
  done
  return tsResults
    where
      drawGraphs 0 ft _  _  log rs= pure (log [], rs [])
      drawGraphs n ft dt ch log rs= do
        (lg0,(zR,lg)) <- liftA2 (,)
                        (quickLog ("Graphing" <> Data.Text.pack (show n)))
                        (do{Async.readChan ch >>= \g -> 
                             tick dt (uncurry (flip graph) . (^. _Right) $ g) >>= 
                             (\lg -> (quickLog ("Done Graphing" <> Data.Text.pack (show n))) >>= \b -> 
                               pure (g,(lg . (b:))))
                           })
        drawGraphs (pred n) ft dt ch (log . (lg0:) . lg) (rs . (zR:))
      {-# INLINE drawGraphs #-}

      blkIO ioQ fT t action = do
        -- Copied from: [2] - just formatted to my preference
        fs@(f:_) <- liftA2 (:) (Async.newEmptyMVar) (Async.takeMVar ioQ)
        Async.putMVar ioQ fs                                            
        tick fT (Async.forkIO (tick t action `finally` Async.putMVar f (snd t)))
      {-# INLINE blkIO #-}


      waitIO ioQ log c = do
        q <- Async.takeMVar ioQ
        case q of
          []     -> return (log []) -- extract difference list
          (f:fs) -> Async.putMVar ioQ fs                                                            *>
                    (quickLog ("Waiting on finaliser" <> (Data.Text.pack (show c))))                >>= \lg0 -> 
                    Async.takeMVar f                                                                >>= \ioT ->
                    (quickLog ("Done on finaliser" <> (Data.Text.pack (show c ++ " " ++ show ioT))))>>= \lg1 ->
                    waitIO ioQ (log . (lg0:) . (lg1:)) (succ c)
      {-# INLINE waitIO #-}

queries = [(minBound :: IndicatorCode) .. ] -- let's see what all the tables look like
testProgram = (zip (repeat $ pg' edgewaterFL)   queries) -- pull latest info on edgewater
               ++ (zip (repeat $ pg' brickellFL )   queries) -- pull latest info on brickell
               ++ (zip (repeat $ pg' losAngelesCA ) queries) -- pull latest info on LA
               ++ (zip (repeat $ pg' expositionCA ) queries) -- pull latest info on Exposition,LA
               ++ (zip (repeat $ pg' c0 ) queries)

data LogEntry = IOLog Text
              | Info Text
              | Error Text deriving Show

instance Monoid LogEntry where
  mempty = Main.Error mempty -- :)
  IOLog l `mappend` IOLog r = IOLog (l <> r)
  Info l `mappend` Info r = Info (l <> r)
  Main.Error l `mappend` Main.Error r = Main.Error (l <> r)

class TextLike a where 
  toText :: (Text -> Text) -> a -> a

instance TextLike LogEntry where
  toText f (IOLog t) = IOLog (f t)
  toText f (Main.Error t) = Main.Error (f t)
  toText f (Info t) = Info (f t)
  {-# INLINE toText #-}

data IO_T = NetworkIO | ForkIO | DrawIO deriving Show

test = runZillowM (testProgram)
test2 = liftA2 (<>) (runZillowM testProgram) (runZillowM testProgram)
main = do
  (_,ST'{..}) <- test
  mapM print tsLogs
  mapM print tsResults
-- see:  -- https://mail.haskell.org/pipermail/glasgow-haskell-users/2012-July/022651.html
-- see:  -- https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Concurrent.html note on Pre-Emption
