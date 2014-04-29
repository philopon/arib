{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}

module Data.Arib.TS.Internal where

import Control.Monad
import Control.Exception
import Control.Applicative
import Control.Monad.Trans.Resource
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Word
import Data.Function
import Data.List
import Data.Bits
import Data.Typeable
import qualified Data.IntMap.Strict as IM
import Debug.Trace

import Data.Conduit
import Numeric
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as CL

newtype TS = TS { tsPacket :: S.ByteString }

instance Show TS where
    show ts =
        "TS {transportErrorIndicator = "    ++ show (transportErrorIndicator ts) ++
        ", payloadUnitStartIndicator = "    ++ show (payloadUnitStartIndicator ts) ++
        ", transportPriority = "            ++ show (transportPriority ts) ++
        ", tsProgramId = 0x"                ++ showHex (tsProgramId ts)
        ", tsTransportScramblingControl = " ++ show (tsTransportScramblingControl ts) ++
        ", adaptationFieldControl = "       ++ show (adaptationFieldControl ts) ++
        ", continuityCounter = 0x"          ++ showHex (continuityCounter ts)
        ", tsPayload = "                    ++ show (tsPayload ts) ++
        "}"                                            
                                                       
transportErrorIndicator, payloadUnitStartIndicator, transportPriority,
    hasAdaptationField, hasPayload :: TS -> Bool
transportErrorIndicator   TS{tsPacket} = testBit (S.index tsPacket 1) 7
payloadUnitStartIndicator TS{tsPacket} = testBit (S.index tsPacket 1) 6
transportPriority         TS{tsPacket} = testBit (S.index tsPacket 1) 5
hasAdaptationField        TS{tsPacket} = testBit (S.index tsPacket 3) 5
hasPayload                TS{tsPacket} = testBit (S.index tsPacket 3) 4
{-# INLINE transportErrorIndicator   #-}
{-# INLINE payloadUnitStartIndicator #-}
{-# INLINE transportPriority         #-}
{-# INLINE hasAdaptationField        #-}
{-# INLINE hasPayload                #-}

tsProgramId :: TS -> Int
tsProgramId TS{tsPacket} =
    shiftL (fromIntegral $ (S.index tsPacket 1) .&. 0x1F) 8 .|. fromIntegral (S.index tsPacket 2)
{-# INLINE tsProgramId #-}

tsTransportScramblingControl, adaptationFieldControl, continuityCounter :: TS -> Word8
tsTransportScramblingControl TS{tsPacket} = shiftR (S.index tsPacket 3) 6
adaptationFieldControl       TS{tsPacket} = shiftR (S.index tsPacket 3) 4 .&. 0x3
continuityCounter            TS{tsPacket} =        (S.index tsPacket 3)   .&. 0xF
{-# INLINE tsTransportScramblingControl #-}
{-# INLINE adaptationFieldControl #-}
{-# INLINE continuityCounter #-}

tsPayload :: TS -> S.ByteString
tsPayload = S.drop 4 . tsPacket
{-# INLINE tsPayload #-}

data TsException
    = ReSyncFailed
    deriving (Show,Typeable)
instance Exception TsException

tsPackets :: MonadThrow m => Int -> Conduit S.ByteString m TS
tsPackets n = {-# SCC "tsPackets" #-} go S.empty
  where
    go beforePayload = do
        rawPacket <- {-# SCC "tsPackets[take]" #-} L.toStrict <$> CB.take n
        when (S.length rawPacket == n) $ do
            packet <- {-# SCC "tsPackets[resync]" #-} resync n beforePayload rawPacket

            yield $ TS packet
            go packet
{-# INLINE tsPackets #-}

safeIndexS :: S.ByteString -> Int -> Maybe Word8
safeIndexS b n
    | n < 0           = Nothing
    | n >= S.length b = Nothing
    | otherwise       = Just $ S.index b n
{-# INLINE safeIndexS #-}

resync :: MonadThrow m
       => Int -> S.ByteString -> S.ByteString -> ConduitM S.ByteString a m S.ByteString
resync len bp rp
    | S.head rp == 0x47 = return rp
    | otherwise         = go 1
  where
    bpLen = S.length bp
    go i
        | safeIndexS bp (bpLen - i) == Just 0x47 = do
            let (res,lo) = S.splitAt len $ (S.drop (bpLen - i) bp) `S.append` rp
            leftover lo
            return res

        | safeIndexS rp (i - 1) == Just 0x47 =
            (S.drop (i - 1) rp `S.append`) <$> (L.toStrict <$> CB.take (i - 1))

        | otherwise = if len > i then go (i + 1) else monadThrow ReSyncFailed

detectPacketSize :: Monad m => Consumer S.ByteString m Int
detectPacketSize = CL.peek >>= \case
    Nothing -> return 0
    Just c  -> do
        let m1  = map (succ . S.length) . tail $ S.split 0x47 c
            m2  = snd $ mapAccumL (\a i -> (i, a+i)) 0 m1
            m1c = foldl' (\d i -> IM.insertWith (+) i 1 d) IM.empty m1
            m2c = IM.map (`quot` 2) $ foldl' (\d i -> IM.insertWith (+) i 1 d) IM.empty m2
            m   = IM.filterWithKey (\k _ -> 188 <= k) . IM.insertWith (flip const) 188 1 $ IM.union m1c m2c
        return . fst . maximumBy (compare `on` snd) $ IM.toList (m :: IM.IntMap Int)

sourceTs :: MonadResource m => FilePath -> Producer m TS
sourceTs file = {-# SCC "sourceTs" #-} CB.sourceFile file $= do
    size <- detectPacketSize
    tsPackets size
{-# INLINE sourceTs #-}

sinkTs :: MonadResource m => FilePath -> Consumer TS m ()
sinkTs file = {-# SCC "sinkTs" #-} CL.map tsPacket =$ CB.sinkFile file

conduitTs :: MonadResource m => FilePath -> Conduit TS m TS
conduitTs file = {-# SCC "conduitTs" #-} CL.map tsPacket =$= CB.conduitFile file =$= CL.map TS

