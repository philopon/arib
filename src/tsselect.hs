{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import qualified Data.Conduit.List as CL
import System.Environment
import qualified Data.IntMap.Strict as IM
import Data.Monoid
import Data.Word
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
            , cc        :: {-#UNPACK#-}!Word8
            } deriving Show

isNextOf :: Word8 -> Word8 -> Bool
0x0 `isNextOf` 0xf = True
0x0 `isNextOf` _   = False
a   `isNextOf` b   = a == b + 1

instance Monoid Count where
    mempty = Count 0 0 0 0 0 0
    Count at ad ae as _ ac `mappend` Count bt bd be bs bo bc =
        let d = if ac `isNextOf` bc then 0 else 1
            in Count (at + bt) (ad + bd + d) (ae + be) (as + bs) bo ac

tsToCount :: TS a -> Int -> Count
tsToCount ts o = 
    Count { total     = 1
          , drops     = 0
          , errors    = if transportErrorIndicator ts then 1 else 0
          , scrambles = if tsTransportScramblingControl ts /= 0 then 1 else 0 
          , offset    = o 
          , cc        = continuityCounter ts
          }

abstruct :: (IM.IntMap Count, Int) -> TS a -> (IM.IntMap Count, Int)
abstruct (!m,!c) ts = (IM.insertWith mappend (tsProgramId ts) (tsToCount ts c) m, succ c)
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

main :: IO ()
main = getArgs >>= \case
    [file] -> runResourceT $ do
        r <- fmap fst $ sourceTs file $$ CL.fold abstruct (IM.empty, 0)
        liftIO $ mapM_ L.putStrLn . showResult $ IM.toAscList r
