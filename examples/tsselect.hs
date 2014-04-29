{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource

import System.Environment
import System.Exit
import System.Directory
import System.IO

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.IntMap.Strict as IM
import Data.Semigroup
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Numeric

import Data.Arib

data Count 
    = Count { total     :: {-#UNPACK#-}!Int
            , drops     :: {-#UNPACK#-}!Int
            , errors    :: {-#UNPACK#-}!Int
            , scrambles :: {-#UNPACK#-}!Int
            , offset    :: {-#UNPACK#-}!Int
            , rawTs     :: {-#UNPACK#-}!TS
            } deriving Show

instance Semigroup Count where
    Count at ad ae as _ ar <> Count bt bd be bs bo br =
        let d = if ar `isNextOf` continuityCounter br then 0 else 1
            in Count (at + bt) (ad + bd + d) (ae + be) (as + bs) bo ar

tsToCount :: TS -> Int -> Count
tsToCount ts o = 
    Count { total     = 1
          , drops     = 0
          , errors    = if transportErrorIndicator ts then 1 else 0
          , scrambles = if tsTransportScramblingControl ts /= 0 then 1 else 0 
          , offset    = o 
          , rawTs     = ts
          }

abstruct :: (IM.IntMap Count, Int) -> TS -> (IM.IntMap Count, Int)
abstruct (!m,!c) ts =  {-# SCC "abstruct" #-}(IM.insertWith (<>) (tsProgramId ts) (tsToCount ts c) m, succ c)
{-# INLINE abstruct #-}

showResult :: [(Int, Count)] -> [L.Text]
showResult l = map (\(pid,c) -> 
    "pid=0x"        `L.append` L.justifyRight 4 '0' (L.pack $ showHex pid []) `L.append`
    ", total="      `L.append` showElem c total `L.append`
    ", d="          `L.append` showElem c drops `L.append`
    ", e="          `L.append` showElem c errors `L.append`
    ", scrambling=" `L.append` showElem c scrambles `L.append`
    ", offset="     `L.append` showElem c offset
    ) l
  where
    maxDigit   f = (+1) . floor . logBase 10 $ (fromIntegral . maximum $ map (f . snd) l :: Double)
    showElem c f = L.justifyRight (maxDigit f) ' ' (L.pack . show $ f c)

select :: Monad m => Bool -> [Int] -> Conduit TS m TS
select include pids = CL.filter (elm pids . tsProgramId)
  where
    elm = flip $ if include then elem else notElem

main :: IO ()
main = getArgs >>= \case
    [file] -> runResourceT $ do
        r <- fmap fst $ sourceTs file $$ CL.fold abstruct (IM.empty, 0)
        liftIO $ mapM_ L.putStrLn . showResult $ IM.toAscList r
    src:dst:pidss_ -> do
        let (include, pidss) = span (== "-x") pidss_ 
        ex <- doesFileExist dst
        when ex $ hPutStrLn stderr "dst file already exists." >> exitFailure
        pids <- mapM readIO pidss :: IO [Int]
        runResourceT $ sourceTs src $$ select (null include) pids =$ sinkTs dst
    _ -> do
        p <- getProgName
        putStrLn $ "USAGE: " ++ p ++ " SRC [DST [-x] pid1 [pid2 ...]]"
