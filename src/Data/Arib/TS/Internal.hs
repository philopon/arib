{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Arib.TS.Internal where

import Control.Monad
import Control.Exception
import Control.Applicative
import Control.Monad.Trans.Resource
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Word
import Data.Function
import Data.List
import Data.Bits
import Data.Monoid
import Data.Typeable
import qualified Data.IntMap.Strict as IM

import Data.Conduit
import Numeric
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as CL

data TS a = 
    TS { header1   :: {-#UNPACK#-}!Word8
       , header2   :: {-#UNPACK#-}!Word8
       , header3   :: {-#UNPACK#-}!Word8
       , tsPayload :: a
       } deriving Functor

instance Show a => Show (TS a) where
    show ts@TS{tsPayload} =
        "TS {transportErrorIndicator = "    ++ show (transportErrorIndicator ts) ++
        ", payloadUnitStartIndicator = "    ++ show (payloadUnitStartIndicator ts) ++
        ", transportPriority = "            ++ show (transportPriority ts) ++
        ", tsProgramId = 0x"                ++ showHex (tsProgramId ts)
        ", tsTransportScramblingControl = " ++ show (tsTransportScramblingControl ts) ++
        ", adaptationFieldControl = "       ++ show (adaptationFieldControl ts) ++
        ", continuityCounter = 0x"          ++ showHex (continuityCounter ts)
        ", tsPayload = "                    ++ show tsPayload ++
        "}"                                            
                                                       
instance Monoid a => Monoid (TS a) where
    mempty = TS 0 0 0 mempty
    TS _ _ _ a `mappend` TS b1 b2 b3 b = TS b1 b2 b3 (a <> b)

transportErrorIndicator, payloadUnitStartIndicator, transportPriority,
    hasAdaptationField, hasPayload :: TS a -> Bool
transportErrorIndicator   TS{header1} = testBit header1 7
payloadUnitStartIndicator TS{header1} = testBit header1 6
transportPriority         TS{header1} = testBit header1 5
hasAdaptationField        TS{header3} = testBit header3 5
hasPayload                TS{header3} = testBit header3 4

tsProgramId :: TS a -> Int
tsProgramId TS{header1, header2} =
    shiftL (fromIntegral $ header1 .&. 0x1F) 8 .|. fromIntegral header2

tsTransportScramblingControl, adaptationFieldControl, continuityCounter :: TS a -> Word8
tsTransportScramblingControl TS{header3} = shiftR header3 6
adaptationFieldControl       TS{header3} = shiftR header3 4 .&. 0x3
continuityCounter            TS{header3} = header3 .&. 0xF

data TsException
    = ReSyncFailed
    deriving (Show,Typeable)
instance Exception TsException

tsPacket :: MonadThrow m => Int -> Conduit S.ByteString m (TS S.ByteString)
tsPacket n = {-# SCC "tsPacket" #-} go S.empty
  where
    n64 :: Int64
    n64 = fromIntegral n

    go beforePayload = do
        rawPacket <- {-# SCC "tsPacket[take]" #-} CB.take n
        when (L.length rawPacket == n64) $ do
            packet <- {-# SCC "tsPacket[resync]" #-} resync n64 beforePayload rawPacket

            let (h,p) = {-# SCC "tsPacket[splitHeader]" #-} L.splitAt 4 packet
            let [_,h1,h2,h3] = L.unpack h

            yield $ TS h1 h2 h3 (L.toStrict p)
            go (L.toStrict p)
{-# INLINE tsPacket #-}

safeIndexL :: L.ByteString -> Int64 -> Maybe Word8
safeIndexL b n
    | n < 0           = Nothing
    | n >= L.length b = Nothing
    | otherwise       = Just $ L.index b n

safeIndexS :: S.ByteString -> Int -> Maybe Word8
safeIndexS b n
    | n < 0           = Nothing
    | n >= S.length b = Nothing
    | otherwise       = Just $ S.index b n

resync :: MonadThrow m
       => Int64 -> S.ByteString -> L.ByteString -> ConduitM S.ByteString a m L.ByteString
resync len bp rp
    | L.head rp == 0x47 = return rp
    | otherwise         = go 1
  where
    bpLen = S.length bp
    go i
        | safeIndexS bp (bpLen - i) == Just 0x47 = do
            let (res,lo) = L.splitAt len $ L.fromStrict (S.drop (bpLen - i) bp) `L.append` rp
            leftover (L.toStrict lo)
            return res

        | safeIndexL rp (fromIntegral i - 1) == Just 0x47 =
            (L.drop (fromIntegral i - 1) rp `L.append`) <$> CB.take (i - 1)

        | otherwise = if len > fromIntegral i then go (i + 1) else monadThrow ReSyncFailed

detectPacketSize :: Monad m => Consumer S.ByteString m Int
detectPacketSize = CL.peek >>= \case
    Nothing -> return 0
    Just c  -> do
        let m = foldl' (\i a -> IM.insertWith (const succ) (fromIntegral $ S.length a) 1 i) IM.empty $ S.split 0x47 c
        return . succ . fst . maximumBy (compare `on` snd) $ IM.toList (m :: IM.IntMap Int)

